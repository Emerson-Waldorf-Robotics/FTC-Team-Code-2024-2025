package org.firstinspires.ftc.teamcode.shared;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import androidx.annotation.NonNull;

import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;
import com.qualcomm.robotcore.hardware.HardwareMap;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.hardware.TouchSensor;
import com.qualcomm.robotcore.util.ElapsedTime;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

public class Shared {
    // Declare OpMode members for each of the 4 motors.
    public static final ElapsedTime runtime = new ElapsedTime();
    public static DcMotorEx leftFrontDrive = null;
    public static DcMotorEx leftBackDrive = null;
    public static DcMotorEx rightFrontDrive = null;
    public static DcMotorEx rightBackDrive = null;

    // Horizontal Extension Motor
    public static DcMotorEx extend_horiz = null;
    // Vertical Extension Motor
    public static DcMotorEx extend_vert  = null;
    public static Servo pivot = null;
    public static Servo clamp = null;
    public static Servo flip  = null;
    public static TouchSensor touch = null;

    public static int[] motorStartPositions = null;

    public static final int EXTEND_DIFFERENCE = 500;
    public static final int VERTICAL_DIFFERENCE = 1830;


    public static HashMap<String, Boolean> actions = null;

    public static ConcurrentHashMap<Long, Runnable> callbacks = null;
    public static ConcurrentHashMap<Supplier<Boolean>, Runnable> checking_callbacks = null;


    private static Telemetry telemetry = null;

    private static Supplier<Boolean> opModeIsActive = null;

    public static void hardwareInit(HardwareMap hardwareMap, Telemetry tel, Supplier<Boolean> op) {
        // Initalize Callbacks (Fix HUGE bug where callbacks persist through opmodes)
        callbacks          = new ConcurrentHashMap<>();
        checking_callbacks = new ConcurrentHashMap<>();

        // Reset runtime too
        runtime.reset();

        // Probably a good idea to re-init a few other things too
        actions = new HashMap<>(16);


        telemetry = tel;
        opModeIsActive = op;

        // Driving stuff
        leftFrontDrive  = hardwareMap.get(DcMotorEx.class, "left_front");
        leftBackDrive  = hardwareMap.get(DcMotorEx.class, "left_back");
        rightFrontDrive = hardwareMap.get(DcMotorEx.class, "right_front");
        rightBackDrive = hardwareMap.get(DcMotorEx.class, "right_back");

        touch = hardwareMap.get(TouchSensor.class, "touch_button");

        motorStartPositions = new int[]{
                leftBackDrive.getCurrentPosition(),
                rightBackDrive.getCurrentPosition(),
                leftFrontDrive.getCurrentPosition(),
                rightFrontDrive.getCurrentPosition()
        };

        leftFrontDrive.setDirection(DcMotor.Direction.FORWARD);
        leftBackDrive.setDirection(DcMotor.Direction.FORWARD);
        rightFrontDrive.setDirection(DcMotor.Direction.REVERSE);
        rightBackDrive.setDirection(DcMotor.Direction.REVERSE);


        // Initialize the hardware variables.
        pivot = hardwareMap.get(Servo.class, "pivot_servo");
        //pivot.scaleRange(0.015, 0.87);
        //pivot.scaleRange(0, 0.8);
        pivot.scaleRange(0, 0.71);

        clamp = hardwareMap.get(Servo.class, "clamp_servo");
        clamp.scaleRange(0.6, 0.85);

        flip  = hardwareMap.get(Servo.class, "flipper_servo");
        flip.scaleRange(0.16, 1);

        extend_horiz  =  hardwareMap.get(DcMotorEx.class, "extend_horizontal");
        extend_vert   =  hardwareMap.get(DcMotorEx.class, "extend_vertical");
    }

    public static void MoveMotor(int where, @NonNull DcMotorEx motor, boolean exact, int vel){
        telemetry.addLine("Running motor...");
        telemetry.addLine("--------------------------------------------------");
        telemetry.addData("Moving by", "%d", where);

        if (exact){
            telemetry.update();

            motor.setTargetPosition(where);
        } else {
            int spos = motor.getCurrentPosition();
            int epos = spos + where;
            telemetry.addData("Start position", "%d", spos);
            telemetry.addData("End Position", "%d", epos);
            telemetry.update();

            motor.setTargetPosition(epos);
        }

        motor.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        motor.setVelocity(vel);
    }

    public static boolean isTrue(Boolean totest){
        return Boolean.TRUE.equals(totest);
    }

    /// Reset the pivot to a "good" position
    public static void ResetPivot(){
        PivotPos(0);
    }

    /// Set the pivot to a position
    public static void PivotPos(int position){
        switch (position) {
            case 0:
                // Ready
                pivot.setPosition(0.65);
                break;
            case 1:
                // Up
                pivot.setPosition(1);
                break;
            case 2:
                // DOwn
                pivot.setPosition(0);
                break;
            case 3:
                // Close to down
                pivot.setPosition(0.2);
                break;
        }
    }

    /// Easier to read way of checking if a button is toggled
    public static boolean isToggled(String value){
        return isTrue(actions.get(value));
    }

    /// Vertical extension
    public static void Extend_Vert(boolean up){
        if (up){
            actions.put("vert", true);

            // Safeties:
            // Are we currently horizontally retracted?
            if (!isToggled("horiz")){
                // Unclamp in case driver forgot to
                Clamp(false);

                //sleep(10);

                // Move the Clamp out of the way
                //ResetPivot();

                // Move clamp away in 10 millis
                registerCallback(Shared::ResetPivot, 10);

                // Extend up in 100 Millis
                registerCallback(() -> MoveMotor(VERTICAL_DIFFERENCE, extend_vert, true, 7000), 100);
            } else {
                // Extend up
                MoveMotor(VERTICAL_DIFFERENCE, extend_vert, true, 7000);

                // Uncomment to slow down the bot a bit but make it so that you can't flip while the arm is still rising
                //sleep(200);
            }
        } else {
            actions.put("vert", false);

            // Down again
            MoveMotor(0, extend_vert, true, 1500);

            // Safety unflip during lower
            Flip(false);

            // Move the Clamp out of the way
            ResetPivot();
        }
    }


    /// Horizontal extension
    public static void Extend_Hori(boolean goout){
        if (goout){
            actions.put("horiz", true);

            // Open clamp
            Clamp(false);

            // Pivot down
            PivotPos(2);
            // Extend
            MoveMotor(EXTEND_DIFFERENCE, extend_horiz, true, 2000);
        } else {
            actions.put("horiz", false);

//            if (isToggled("vert")){
//                // Quickly Bring bucket down
//                Extend_Vert(false);
//                //MoveMotorTel(0, extend_vert, true, 2500);
//                //actions.put("vert", Boolean.FALSE);
//            }

            // Pivot up
            PivotPos(1);
            // Retract
            MoveMotor(0, extend_horiz, true, 2000);

            // Delay
            //sleep(750);

            // Unclamp
            //Clamp(false);

            // Unclamp in 750 ms
            //registerCallback(() -> Clamp(false), 750);
            // TODO: Cursed
            registerCheckingCallback(() -> registerCallback(() -> Clamp(false), 500), () -> touch.isPressed());
        }
    }

    /// Clamp the clamp
    public static void Clamp(boolean on){
        if (on){
            actions.put("clamp", true);
            // Clamp
            clamp.setPosition(0);
        } else {
            actions.put("clamp", false);
            // Unclamp
            clamp.setPosition(1);
        }
    }


    /// Flip the flipper
    public static void Flip(boolean up){
        if (up){
            // Don't flip while lowered
            if (isToggled("vert")){
                actions.put("flip", true);
                // Flip
                flip.setPosition(1);

                //Extend_Hori(false);
                //ResetPivot();
            } else {
                telemetry.addLine("Won't flip while lowered.");
            }
        } else {
            actions.put("flip", false);
            // Unflip
            flip.setPosition(0);
        }
    }


    /// Initialize servos to start position to hold them there
    public static void initMotors(){
        // Pivot reset position
        ResetPivot();

        // Flip reset position
        Flip(false);

        extend_horiz.setMode(DcMotor.RunMode.RUN_WITHOUT_ENCODER);
        extend_horiz.setPower(-0.5);
        while (!touch.isPressed()){
            if (!opModeIsActive.get()){
                return;
            }
        }
        extend_horiz.setPower(0);


        extend_horiz.setMode(DcMotorEx.RunMode.STOP_AND_RESET_ENCODER);
        extend_vert.setMode(DcMotorEx.RunMode.STOP_AND_RESET_ENCODER);
    }



    // Callbacks

    /// Register a callback to be ran later
    public static void registerCallback(Runnable callback, int delayMillis){
        // Get current time in nanoseconds
        long now = runtime.nanoseconds();

        // Calculate when the callback should be run
        long runwhen = now + (delayMillis * 1000000L);

        // Add it to the callbacks HashMap
        callbacks.put(runwhen, callback);
    }

    /// Register a callback to be ran later
    public static void registerCheckingCallback(Runnable callback, Supplier<Boolean> check){
        // If check passes, run right now
        if (check.get()) {
            callback.run();
            return;
        }

        // Add it to the callbacks HashMap
        checking_callbacks.put(check, callback);
    }

    /// Run and delete all callbacks that have expired
    public static void runCallbacks(){
        // Get current (run)time in nanoseconds
        long now = runtime.nanoseconds();

        // List of id's to remove
        ArrayList<Long> toRemove = new ArrayList<>();
        ArrayList<Supplier<Boolean>> toRemoveCheck = new ArrayList<>();

        if (!callbacks.isEmpty()) {
            telemetry.addLine("Callbacks:");
        }
        for (Map.Entry<Long, Runnable> entry : callbacks.entrySet()){
            telemetry.addData("\t", "%d: %s", (entry.getKey() - now) / 1000000L, entry.getValue().toString());

            // Check if time has expired
            if (entry.getKey() - now <= 0) {
                // Run the callback
                entry.getValue().run();

                toRemove.add(entry.getKey());
            }
        }

        for (long id : toRemove){
            callbacks.remove(id);
        }

        for (Map.Entry<Supplier<Boolean>, Runnable> entry : checking_callbacks.entrySet()){
            telemetry.addData("\t", "%s: %s", entry.getKey().toString(), entry.getValue().toString());

            if (entry.getKey().get()) {
                // Run the callback
                entry.getValue().run();

                toRemoveCheck.add(entry.getKey());
            }
        }

        for (Supplier<Boolean> id : toRemoveCheck){
            checking_callbacks.remove(id);
        }
    }

    // Automatic Movement
    public static final int[] motpos = {
            // LFront
            0,
            // LBack
            0,
            // RFront
            0,
            // RBack
            0
    };

    public static void forward(int howmuch){
        motpos[0] += howmuch;
        motpos[1] += howmuch;
        motpos[2] += howmuch;
        motpos[3] += howmuch;
    }

    public static void backwards(int howmuch){
        forward(-howmuch);
    }

    public static void turnRight(int degrees){
        turnLeft(-degrees);
    }

    public static void turnLeft(int degrees){
        motpos[0] += degrees;
        motpos[1] += degrees;
        motpos[2] -= degrees;
        motpos[3] -= degrees;
    }

    public static void strafeRight(int howmuch){
        motpos[0] += howmuch;
        motpos[1] -= howmuch;
        motpos[2] -= howmuch;
        motpos[3] += howmuch;
    }

    public static void strafeLeft(int howmuch){
        strafeRight(-howmuch);
    }

    public static void motoGO(double vel){
        leftFrontDrive.setTargetPosition(motpos[0]);
        leftBackDrive.setTargetPosition(motpos[1]);
        rightFrontDrive.setTargetPosition(motpos[2]);
        rightBackDrive.setTargetPosition(motpos[3]);

        leftFrontDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        leftBackDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        rightBackDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        rightFrontDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);

        leftFrontDrive.setVelocity(vel);
        leftBackDrive.setVelocity(vel);
        rightBackDrive.setVelocity(vel);
        rightFrontDrive.setVelocity(vel);
    }

    public static void waitMoveDone(){
        while (motorBusy()) {}
    }

    public static boolean motorBusy(){
        return leftFrontDrive.isBusy() || leftBackDrive.isBusy() || rightBackDrive.isBusy() || rightFrontDrive.isBusy();
    }
}
