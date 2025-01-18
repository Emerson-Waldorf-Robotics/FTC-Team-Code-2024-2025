package org.firstinspires.ftc.teamcode;

import androidx.annotation.NonNull;

import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.hardware.TouchSensor;
import com.qualcomm.robotcore.util.ElapsedTime;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

import static org.firstinspires.ftc.teamcode.shared.Shared.MoveMotor;


// AMain for appearing on top if alphabetical
@TeleOp(name="Run TeleOp", group = "AMain")
public class RunFullTeleOp extends LinearOpMode
{
    /** Manipulator:
     * 0: Invalid
     * 1: Jacob
     * 2: Dario
     * 4: Camilo
     */
    private char MANIPULATOR = 0;

    static class Qol {
        // Last button position
        static HashMap<String, Boolean> buttonStates = new HashMap<>(4);


        static boolean checkButton(boolean button, String buttonName) {
            // Set false if we dont have a value
            buttonStates.putIfAbsent(buttonName, false);

            if (button) {
                // If we were already pressing the button
                if (Boolean.FALSE.equals(buttonStates.get(buttonName))) {
                    buttonStates.put(buttonName, true);
                    return true;
                }
            } else {
                buttonStates.put(buttonName, false);
            }
            return false;
        }
    }

    // Move motor proxy to shared code
    void MoveMotorTel(int where, @NonNull DcMotorEx motor, boolean exact, int vel){
        MoveMotor(where, motor, exact, vel, telemetry);
    }

    HashMap<String, Boolean> action1 = new HashMap<>(16);

    ConcurrentHashMap<Long, Runnable> callbacks = new ConcurrentHashMap<>();
    ConcurrentHashMap<Supplier<Boolean>, Runnable> checking_callbacks = new ConcurrentHashMap<>();

    boolean isTrue(Boolean totest){
        return Boolean.TRUE.equals(totest);
    }



    // Horizontal Extension Motor
    private DcMotorEx extend_horiz = null;
    // Vertical Extension Motor
    private DcMotorEx extend_vert  = null;
    private Servo pivot = null;
    private Servo clamp = null;
    private Servo flip  = null;


    // Driving stuff
    private ElapsedTime runtime = new ElapsedTime();
    private DcMotor leftFrontDrive = null;
    private DcMotor leftBackDrive = null;
    private DcMotor rightFrontDrive = null;
    private DcMotor rightBackDrive = null;

    private TouchSensor touch = null;


    private final int EXTEND_DIFFERENCE = 500;
    private final int VERTICAL_DIFFERENCE = 1830;

    @Override public void runOpMode()
    {
        telemetry.addLine("Please select manipulator for control selection:");
        telemetry.addLine("On Gamepad 1 Press:");
        telemetry.addLine("Botton button: Jacob");
        telemetry.addLine("Top button:    Dario");
        telemetry.addLine("Left Button:   Camilo");
        telemetry.update();
        while (opModeInInit()){
            if (gamepad1.a){
                MANIPULATOR = 2;
                break;
            }
            if (gamepad1.y){
                MANIPULATOR = 2;
                break;
            }
            if (gamepad1.x){
                MANIPULATOR = 4;
                break;
            }
        }
        if (!opModeInInit()){
            return;
        }

        // Driving stuff
        leftFrontDrive  = hardwareMap.get(DcMotor.class, "left_front");
        leftBackDrive  = hardwareMap.get(DcMotor.class, "left_back");
        rightFrontDrive = hardwareMap.get(DcMotor.class, "right_front");
        rightBackDrive = hardwareMap.get(DcMotor.class, "right_back");

        touch = hardwareMap.get(TouchSensor.class, "touch_button");

        int[] startPositions = {
                leftBackDrive.getCurrentPosition(),
                rightBackDrive.getCurrentPosition(),
                leftFrontDrive.getCurrentPosition(),
                rightFrontDrive.getCurrentPosition()
        };

        leftFrontDrive.setDirection(DcMotor.Direction.FORWARD);
        leftBackDrive.setDirection(DcMotor.Direction.FORWARD);
        rightFrontDrive.setDirection(DcMotor.Direction.REVERSE);
        rightBackDrive.setDirection(DcMotor.Direction.REVERSE);


        // Initialize the hardware variables. Note that the strings used here as parameters
        // to 'get' must match the names assigned during the robot configuration.
        // step (using the FTC Robot Controller app on the phone).
        pivot = hardwareMap.get(Servo.class, "pivot_servo");
        // Vertical: 0.82
        pivot.scaleRange(0.015, 0.87);
        //pivot.setDirection(Servo.Direction.REVERSE);
        clamp = hardwareMap.get(Servo.class, "clamp_servo");
        clamp.scaleRange(0.6, 0.85);
        flip  = hardwareMap.get(Servo.class, "flipper_servo");
        flip.scaleRange(0.16, 1);

        extend_horiz  =  hardwareMap.get(DcMotorEx.class, "extend_horizontal");
        extend_vert   =  hardwareMap.get(DcMotorEx.class, "extend_vertical");

        //telemetry.addData(">", "Touch START to Initialize.");
        //telemetry.update();
        //waitForStart();


        extend_horiz.setMode(DcMotorEx.RunMode.STOP_AND_RESET_ENCODER);
        extend_vert.setMode(DcMotorEx.RunMode.STOP_AND_RESET_ENCODER);

        telemetry.addLine("Controller 1: Manipulator");
        telemetry.addLine("Controller 2: Driver");

        telemetry.addData(">", "Touch START to Start OpMode.");
        telemetry.update();

        waitForStart();
        runtime.reset();

        initMotors();

        extend_horiz.setMode(DcMotorEx.RunMode.STOP_AND_RESET_ENCODER);
        extend_vert.setMode(DcMotorEx.RunMode.STOP_AND_RESET_ENCODER);

        ResetPivot();

        while (opModeIsActive())
        {
            addNeededTelemetry();

            drive(startPositions);

            doActions(startPositions);

            runCallbacks();

            telemetry.update();

            sleep(10);
        }
    }


    /// Vertical extension
    void Extend_Vert(boolean up){
        if (up){
            action1.put("x", true);

            // Safeties:
            // Are we currently horizontally retracted?
            if (!isToggled("a")){
                // Unclamp in case driver forgot to
                Clamp(false);

                //sleep(10);

                // Move the Clamp out of the way
                //ResetPivot();

                // Move clamp away in 10 millis
                registerCallback(this::ResetPivot, 10);

                // Extend up in 100 Millis
                registerCallback(() -> MoveMotorTel(VERTICAL_DIFFERENCE, extend_vert, true, 7000), 100);
            } else {
                // Extend up
                MoveMotorTel(VERTICAL_DIFFERENCE, extend_vert, true, 7000);

                // Uncomment to slow down the bot a bit but make it so that you can't flip while the arm is still rising
                //sleep(200);
            }
        } else {
            action1.put("x", false);

            // Down again
            MoveMotorTel(0, extend_vert, true, 1500);

            // Safety unflip during lower
            Flip(false);

            // Move the Clamp out of the way
            ResetPivot();
        }
    }


    /// Horizontal extension
    void Extend_Hori(boolean goout){
        if (goout){
            action1.put("a", true);

            // Open clamp
            Clamp(false);

            // Pivot down
            PivotPos(2);
            // Extend
            MoveMotorTel(EXTEND_DIFFERENCE, extend_horiz, true, 2000);
        } else {
            action1.put("a", false);

//            if (isToggled("x")){
//                // Quickly Bring bucket down
//                Extend_Vert(false);
//                //MoveMotorTel(0, extend_vert, true, 2500);
//                //action1.put("x", Boolean.FALSE);
//            }

            // Pivot up
            PivotPos(1);
            // Retract
            MoveMotorTel(0, extend_horiz, true, 2000);

            // Delay
            //sleep(750);

            // Unclamp
            //Clamp(false);

            // Unclamp in 750 ms
            //registerCallback(() -> Clamp(false), 750);
            registerCheckingCallback(() -> Clamp(false), () -> touch.isPressed());
        }
    }

    /// Clamp the clamp
    void Clamp(boolean on){
        if (on){
            action1.put("b", true);
            // Clamp
            clamp.setPosition(0);
        } else {
            action1.put("b", false);
            // Unclamp
            clamp.setPosition(1);
        }
    }


    /// Flip the flipper
    void Flip(boolean up){
        if (up){
            // Don't flip while lowered
            if (isToggled("x")){
                action1.put("y", true);
                // Flip
                flip.setPosition(1);

                //Extend_Hori(false);
                //ResetPivot();
            } else {
                telemetry.addLine("Won't flip while lowered.");
            }
        } else {
            action1.put("y", false);
            // Unflip
            flip.setPosition(0);
        }
    }

    /// Easier to read way of checking if a button is toggled
    boolean isToggled(String value){
        return isTrue(action1.get(value));
    }

    /// Reset the pivot to a "good" position
    void ResetPivot(){
        PivotPos(0);
    }

    /// Set the pivot to a position
    void PivotPos(int position){
        switch (position) {
            case 0:
                pivot.setPosition(0.65);
                break;
            case 1:
                pivot.setPosition(1);
                break;
            case 2:
                pivot.setPosition(0);
                break;
            case 3:
                pivot.setPosition(0.2);
                break;
            default:
                // Crash program so we can see where this was called from
                //noinspection divzero,NumericOverflow
                telemetry.addData("Error: Invalid Pivot position", "%d", 1/0);
        }
    }

    /// Manual Linear Extension. Kind of like it's own mini OpMode
    // TODO: Consider using stick pos as extension progress
    void ManualExtend(int[] stp){
        // Initialize button states and toggles.
        // It would be overkill to use another HashMap here
        boolean buttonState1  = false, buttonState2  = false;
        boolean buttonToggle1 = false, buttonToggle2 = false;

        boolean APress = true;

        action1.put("a", true);

        Extend_Vert(false);

        // Open clamp
        Clamp(false);

        // Pivot down
        PivotPos(3);

        // Set to RUN_USING ENCODER
        extend_horiz.setMode(DcMotorEx.RunMode.RUN_USING_ENCODER);
        while (opModeIsActive()){
            if (APress && !gamepad1.a) {
                APress = false;
            }

            if (!APress && gamepad1.a) {
                extend_horiz.setPower(0);
                break;
            }


            // Allow the driver to interact while picking up
            drive(stp);

            // Fun math to figure out whether we will overstep bounds and possibly break the bot by continuing
            float val = -gamepad1.left_stick_y; // Invert it
            int pos = extend_horiz.getCurrentPosition();

            // If manipulator requests to stop moving the arm
            if (val == 0){
                // Stop. It doesn't matter how far out or in it is.
                extend_horiz.setPower(0);

            // If the manipulator requests to move the arm in
            } else if (val < 0){
                // Make sure that we are not already fully in
                if (!(pos < 10)){
                    // Accept the request
                    extend_horiz.setPower(val);
                }

            // If the manipulator requests to move the arm out
            } else {
                // Make sure that we are not already fully out
                if (!(pos > EXTEND_DIFFERENCE - 10)){
                    // Accept the request
                    extend_horiz.setPower(val);
                }
            }

            // If we are about to go too far out
            if (pos > EXTEND_DIFFERENCE - 10){
                // Make sure that we are actually moving
                if (extend_horiz.getPower() > 0) {
                    // Stop the motor
                    extend_horiz.setPower(0);
                }
            }
            // If we are about to go too far in
            if (pos < 10){
                // Make sure that we are actually moving
                if (extend_horiz.getPower() < 0) {
                    // Stop the motor
                    extend_horiz.setPower(0);
                }
            }

            // Camilo likes pivot to be X
            if ((MANIPULATOR == 4)? gamepad1.x : gamepad1.b){
                // Fun stuff to have the button toggle what it does every press
                if (!buttonToggle1){
                    buttonToggle1 = true;

                    if (!buttonState1){
                        buttonState1 = true;

                        // Unclamp if lowering
                        //Clamp(false);

                        // On first press:
                        PivotPos(2);
                    } else {
                        buttonState1 = false;

                        // On second press:
                        PivotPos(3);
                    }
                }
            } else {
                buttonToggle1 = false;
            }

            // Camilo likes clamp to be B
            if ((MANIPULATOR == 4)? gamepad1.b : gamepad1.x){
                // More fun stuff to have the button toggle what it does every press
                if (!buttonToggle2){
                    buttonToggle2 = true;

                    if (!buttonState2){
                        buttonState2 = true;

                        // First press:
                        Clamp(true);
                    } else {
                        buttonState2 = false;

                        // Second press:
                        Clamp(false);
                    }
                }
            } else {
                buttonToggle2 = false;
            }
        }
        // End while loop (A was released or OpMode stopped)

        // Just in case. We don't want to move any motors after the OpMode is requested to be stopped
        if (!opModeIsActive()){
            return;
        }

        // Check if we are already clamped on
        if (isToggled("b")){
            // Bring the Linear Extension Back in
            Extend_Hori(false);
        } else {
            // No good reason to bring back in without clamping on. Clamp on now just in case
            Clamp(true);

            // Retract linear in 300 millis (after clamping)
            registerCallback(() -> Extend_Hori(false), 300);
        }

        action1.put("a", false);
    }

    /// Shortened and slightly modified drive from OmniLinearOpMode
    void drive(@NonNull int[] startPositions){
        // Well, I guess they DO have copyright... so for this function:

        /* Copyright (c) 2021 FIRST. All rights reserved.
         *
         * Redistribution and use in source and binary forms, with or without modification,
         * are permitted (subject to the limitations in the disclaimer below) provided that
         * the following conditions are met:
         *
         * Redistributions of source code must retain the above copyright notice, this list
         * of conditions and the following disclaimer.
         *
         * Redistributions in binary form must reproduce the above copyright notice, this
         * list of conditions and the following disclaimer in the documentation and/or
         * other materials provided with the distribution.
         *
         * Neither the name of FIRST nor the names of its contributors may be used to endorse or
         * promote products derived from this software without specific prior written permission.
         *
         * NO EXPRESS OR IMPLIED LICENSES TO ANY PARTY'S PATENT RIGHTS ARE GRANTED BY THIS
         * LICENSE. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
         * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
         * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
         * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
         * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
         * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
         * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
         * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
         * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
         * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
         */

        int[] positions = {
                leftBackDrive.getCurrentPosition() - startPositions[0],
                rightBackDrive.getCurrentPosition() - startPositions[1],
                leftFrontDrive.getCurrentPosition() - startPositions[2],
                rightFrontDrive.getCurrentPosition() - startPositions[3]
        };

        double max;

        // TODO: Use exponential
        double axial   = -gamepad2.left_stick_y;  // Note: pushing stick forward gives negative value
        double lateral =  gamepad2.left_stick_x;
        double yaw     =  -gamepad2.right_stick_x; // This needs to be negative for some reason?

        double leftFrontPower  = axial + lateral + yaw;
        double rightFrontPower = axial - lateral - yaw;
        double leftBackPower   = axial - lateral + yaw;
        double rightBackPower  = axial + lateral - yaw;


        // Normalize the values so no wheel power exceeds 100%
        // This ensures that the robot maintains the desired motion.
        max = Math.max(Math.abs(leftFrontPower), Math.abs(rightFrontPower));
        max = Math.max(max, Math.abs(leftBackPower));
        max = Math.max(max, Math.abs(rightBackPower));

        // Allow speed to be slowed based on pressure put on the left trigger
        max *= 3-(gamepad2.left_trigger*2);

        if (max > 1.0) {
            leftFrontPower  /= max;
            rightFrontPower /= max;
            leftBackPower   /= max;
            rightBackPower  /= max;
        }

        // Send calculated power to wheels
        leftFrontDrive.setPower(leftFrontPower);
        rightFrontDrive.setPower(rightFrontPower);
        leftBackDrive.setPower(leftBackPower);
        rightBackDrive.setPower(rightBackPower);

        telemetry.addData("Status", "Run Time: " + runtime.toString());
        telemetry.addData("Front left/Right", "%4.2f, %4.4f", leftFrontPower, rightFrontPower);
        telemetry.addData("Back  left/Right", "%4.2f, %4.4f", leftBackPower, rightBackPower);
        telemetry.addData("Back Encoder l/r", "%d, %d", positions[0], positions[1]);
        telemetry.addData("Front Encoder l/r", "%d, %d", positions[2], positions[3]);
    }


    /// Run actions based on buttons pressed on Manipulator's controller
    void doActions(int[] stp){
        if (Qol.checkButton(gamepad1.a, "a")){
            if (isToggled("a") || gamepad1.right_trigger > 0.5){
                Extend_Hori(!isToggled("a"));
            } else {
                ManualExtend(stp);
            }
        }
        if (Qol.checkButton(gamepad1.b, "b"))
            Clamp(!isToggled("b"));

        if (Qol.checkButton(gamepad1.x, "x"))
            Extend_Vert(!isToggled("x"));

        if (Qol.checkButton(gamepad1.y, "y"))
            Flip(!isToggled("y"));
    }


    /// Initialize servos to start position to hold them there
    void initMotors(){
        // Pivot reset position
        ResetPivot();

        // Flip reset position
        Flip(false);

        extend_horiz.setMode(DcMotor.RunMode.RUN_WITHOUT_ENCODER);
        extend_horiz.setPower(-0.3);
        while (!touch.isPressed()){
            if (!opModeIsActive()){
                return;
            }
        }
        extend_horiz.setPower(0);
    }


    // Callbacks

    /// Register a callback to be ran later
    void registerCallback(Runnable callback, int delayMillis){
        // Get current time in nanoseconds
        long now = runtime.nanoseconds();

        // Calculate when the callback should be run
        long runwhen = now + (delayMillis * 1000000L);

        // Add it to the callbacks HashMap
        callbacks.put(runwhen, callback);
    }

    /// Register a callback to be ran later
    void registerCheckingCallback(Runnable callback, Supplier<Boolean> check){
        // If check passes, run right now
        if (check.get()) {
            callback.run();
            return;
        }

        // Add it to the callbacks HashMap
        checking_callbacks.put(check, callback);
    }


    /// Run and delete all callbacks that have expired
    void runCallbacks(){
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

    void addNeededTelemetry(){
        switch (MANIPULATOR){
            //case 1:
            //    telemetry.addData("Manipulator", "Jacob");
            //    break;
            case 2:
                telemetry.addData("Manipulator", "Dario / Jacob");
                break;
            case 4:
                telemetry.addData("Manipulator", "Camilo");
                break;
            default:
                //noinspection divzero,NumericOverflow
                telemetry.addData("Could not determine manipulator, please restart robot", 1/0);
        }

        // Might be needed?
        telemetry.addLine("Controller 1: Manipulator");
        telemetry.addLine("Controller 2: Driver");

        telemetry.addData("Linear extension:", touch.isPressed() ? "In" : "Out");
        telemetry.addData("Clamp", isToggled("b")? "Clamped":"Open");
        telemetry.addData("Lift", isToggled("x")? "Up":"Down");
        telemetry.addData("Flipper", isToggled("y")? "Flipped": "Not Flipped");
    }
}
