package org.firstinspires.ftc.teamcode;

import androidx.annotation.NonNull;

import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.ElapsedTime;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;


// AMain for appearing on top if alphabetical
@TeleOp(name="Run TeleOp", group = "AMain")
public class RunFullTeleOp extends LinearOpMode
{
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

    HashMap<String, Boolean> action1 = new HashMap<>(16);

    HashMap<Long, Runnable> callbacks = new HashMap<>();

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


    private final int EXTEND_DIFFERENCE = 500;
    private final int VERTICAL_DIFFERENCE = 1720;


    void MoveMotor(int where, @NonNull DcMotorEx motor, boolean exact, int vel){
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

    @Override public void runOpMode()
    {
        // Driving stuff
        leftFrontDrive  = hardwareMap.get(DcMotor.class, "left_front");
        leftBackDrive  = hardwareMap.get(DcMotor.class, "left_back");
        rightFrontDrive = hardwareMap.get(DcMotor.class, "right_front");
        rightBackDrive = hardwareMap.get(DcMotor.class, "right_back");

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
        extend_horiz  =  hardwareMap.get(DcMotorEx.class, "extend_horizontal");
        extend_horiz.setMode(DcMotorEx.RunMode.STOP_AND_RESET_ENCODER);
        extend_vert   =  hardwareMap.get(DcMotorEx.class, "extend_vertical");
        extend_vert.setMode(DcMotorEx.RunMode.STOP_AND_RESET_ENCODER);
        pivot = hardwareMap.get(Servo.class, "pivot_servo");
        // Vertical: 0.82
        pivot.scaleRange(0.015, 0.87);
        //pivot.setDirection(Servo.Direction.REVERSE);
        clamp = hardwareMap.get(Servo.class, "clamp_servo");
        clamp.scaleRange(0.6, 0.85);
        flip  = hardwareMap.get(Servo.class, "flipper_servo");
        flip.scaleRange(0.16, 1);

        //telemetry.addData(">", "Touch START to Initialize.");
        //telemetry.update();
        //waitForStart();

        initServos();

        telemetry.addLine("Controller 1: Manipulator");
        telemetry.addLine("Controller 2: Driver");

        telemetry.addData(">", "Touch START to Start OpMode.");
        telemetry.update();

        waitForStart();
        runtime.reset();

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

                // Extend up in 20 Millis
                registerCallback(() -> MoveMotor(VERTICAL_DIFFERENCE, extend_vert, true, 5000), 20);
            } else {
                // Extend up
                MoveMotor(VERTICAL_DIFFERENCE, extend_vert, true, 5000);

                // Uncomment to slow down the bot a bit but make it so that you can't flip while the arm is still rising
                //sleep(200);
            }
        } else {
            action1.put("x", false);

            // Safety unflip during lower
            Flip(false);

            if (!isToggled("a")){
                // Move the Clamp out of the way
                ResetPivot();
            }

            // Down again
            MoveMotor(0, extend_vert, true, 1000);
        }
    }


    /// Horizontal extension
    void Extend_Hori(boolean goout){
        if (goout){
            action1.put("a", true);

            // Open clamp
            Clamp(false);

            // Pivot down
            Pivot(false);
            // Extend
            MoveMotor(EXTEND_DIFFERENCE, extend_horiz, true, 2000);
        } else {
            action1.put("a", false);

            if (isToggled("x")){
                // Quickly Bring bucket down
                Extend_Vert(false);
                //MoveMotor(0, extend_vert, true, 2500);
                //action1.put("x", Boolean.FALSE);
            }

            // Pivot up
            Pivot(true);
            // Retract
            MoveMotor(0, extend_horiz, true, 2000);

            // Delay
            //sleep(750);

            // Unclamp
            //Clamp(false);

            // Unclamp in 750 ms
            registerCallback(() -> Clamp(false), 750);
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
        pivot.setPosition(0.65);
    }

    /*
     * TODO: Use int instead of bool to convey position:
     *  We would need the positions:
     *  Depositing
     *  Down
     *  Close to down (for manual extensions)
     *  Away from lift (for lifting)
     */
    void Pivot(boolean up){
        pivot.setPosition(up ? 1 : 0);
    }

    /// Manual Linear Extension. Kind of like it's own mini OpMode
    // TODO: Consider using stick pos as extension progress
    void ManualExtend(int[] stp){
        // Initialize button states and toggles.
        // It would be overkill to use another HashMap here
        boolean buttonState1  = false, buttonState2  = false;
        boolean buttonToggle1 = false, buttonToggle2 = false;

        action1.put("a", true);

        // Open clamp
        Clamp(false);

        // Pivot down
        //Pivot(false);

        // Get the current motor run mode (Probably RUN_TO_POSITION but you can't be sure)
        DcMotor.RunMode lastmode = extend_horiz.getMode();

        // Set to RUN_USING ENCODER
        extend_horiz.setMode(DcMotorEx.RunMode.RUN_USING_ENCODER);
        while (opModeIsActive()){
            // Allow the driver to interact while picking up
            drive(stp);

            // Fun math to figure out whether we will overstep bounds and possibly break the bot by continuing
            float val = gamepad1.left_stick_y;
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

            // Stop if A released
            if (!gamepad1.a){
                extend_horiz.setPower(0);
                break;
            }

            // B button is now for lowering the pivot
            if (gamepad1.b){
                // Fun stuff to have the button toggle what it does every press
                if (!buttonToggle1){
                    buttonToggle1 = true;

                    if (!buttonState1){
                        buttonState1 = true;

                        // On first press:
                        Pivot(false);
                    } else {
                        buttonState1 = false;

                        // On second press:
                        Pivot(true);
                    }
                }
            } else {
                buttonToggle1 = false;
            }

            // X button now for clamping
            if (gamepad1.x){
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
        // End while loop (A was released or OpMode stopped

        // Just in case. We don't want to move any motors after the OpMode is requested to be stopped
        if (!opModeIsActive()){
            return;
        }

        // Reset the motor mode
        extend_horiz.setMode(lastmode);

        // Check if we are already clamped on
        if (isToggled("b")){
            // Bring the Linear Extension Back in
            // TODO: Consider toggling with A instead of having to hold A
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
        max *= gamepad2.left_trigger*2+1;

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
            if (isToggled("a") || gamepad1.right_trigger < 0.5){
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
    void initServos(){
        // Pivot reset position
        ResetPivot();

        // Flip reset position
        Flip(false);
    }


    // Callbacks
    // TODO: Test these

    /// Register a callback to be ran later
    void registerCallback(Runnable callback, int delayMillis){
        // Get current time in nanoseconds
        long now = runtime.nanoseconds();

        // Calculate when the callback should be run
        long runwhen = now + (delayMillis * 1000000L);

        // Add it to the callbacks HashMap
        callbacks.put(runwhen, callback);
    }


    /// Run and delete all callbacks that have expired
    void runCallbacks(){
        // Get current (run)time in nanoseconds
        long now = runtime.nanoseconds();

        // We do it this way instead of enhanced for as we need to remove items while iterating.
        // TODO: Consider, would an enhanced for that adds to a list and then iterating through that list be better?

        // Create an Iterator over the entryset
        Iterator<Map.Entry<Long, Runnable>> it = callbacks.entrySet().iterator();

        // Loop through callbacks
        while (it.hasNext()){
            // Get the current entry
            Map.Entry<Long, Runnable> entry = it.next();

            // Check if time has expired
            if (entry.getKey() <= now) {
                // Run the callback
                entry.getValue().run();
                // Remove the callback
                it.remove();
            }
        }
    }

    void addNeededTelemetry(){
        // Might be needed?
        telemetry.addLine("Controller 1: Manipulator");
        telemetry.addLine("Controller 2: Driver");
    }
}
