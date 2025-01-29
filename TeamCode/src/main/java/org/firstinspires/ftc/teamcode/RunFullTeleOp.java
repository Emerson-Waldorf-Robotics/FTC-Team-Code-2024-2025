package org.firstinspires.ftc.teamcode;

import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotorEx;

import java.util.HashMap;

import static org.firstinspires.ftc.teamcode.shared.Shared.*;


// AMain for appearing on top if alphabetical
@TeleOp(name="Run TeleOp", group = "AMain")
public class RunFullTeleOp extends LinearOpMode
{
    private boolean speeding = false;

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


    boolean isTrue(Boolean totest){
        return Boolean.TRUE.equals(totest);
    }

    public boolean getOpActive(){
        return opModeIsActive();
    }

    // TODO: Calibrate this
    private static final int LOW_BUCKET_VERTICAL = 224;

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

        hardwareInit(hardwareMap, telemetry, this::getOpActive);

        telemetry.addLine("Controller 1: Manipulator");
        telemetry.addLine("Controller 2: Driver");

        telemetry.addData(">", "Touch START to Start OpMode.");
        telemetry.update();

        waitForStart();
        runtime.reset();

        initMotors();

        ResetPivot();

        while (opModeIsActive())
        {
            addNeededTelemetry();

            drive();

            doActions();

            runCallbacks();

            telemetry.update();

            sleep(10);
        }
    }

    /// Manual Linear Extension. Kind of like it's own mini OpMode
    // TODO: Consider using stick pos as extension progress
    void ManualExtend(){
        // Initialize button states and toggles.
        // It would be overkill to use another HashMap here
        boolean buttonState1  = false, buttonState2  = false;
        boolean buttonToggle1 = false, buttonToggle2 = false;

        boolean APress = true;

        actions.put("horiz", true);

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
            drive();

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
        if (isToggled("clamp")){
            // Bring the Linear Extension Back in
            Extend_Hori(false);
        } else {
            // No good reason to bring back in without clamping on. Clamp on now just in case
            Clamp(true);

            // Retract linear in 300 millis (after clamping)
            registerCallback(() -> Extend_Hori(false), 300);
        }

        actions.put("horiz", false);
    }

    /// Shortened and slightly modified drive from OmniLinearOpMode
    void drive(){
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
                leftBackDrive.getCurrentPosition() - motorStartPositions[0],
                rightBackDrive.getCurrentPosition() - motorStartPositions[1],
                leftFrontDrive.getCurrentPosition() - motorStartPositions[2],
                rightFrontDrive.getCurrentPosition() - motorStartPositions[3]
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

        if (speeding){
            if (leftBackPower != 0 || leftFrontPower != 0 || rightBackPower != 0 || rightFrontPower != 0){
                // Send calculated power to wheels
                leftFrontDrive.setPower(leftFrontPower);
                rightFrontDrive.setPower(rightFrontPower);
                leftBackDrive.setPower(leftBackPower);
                rightBackDrive.setPower(rightBackPower);
                speeding = false;
            }
            if (gamepad2.y){
                // Send calculated power to wheels
                leftFrontDrive.setPower(0);
                rightFrontDrive.setPower(0);
                leftBackDrive.setPower(0);
                rightBackDrive.setPower(0);
                speeding = false;
            }
        } else {
            // Send calculated power to wheels
            leftFrontDrive.setPower(leftFrontPower);
            rightFrontDrive.setPower(rightFrontPower);
            leftBackDrive.setPower(leftBackPower);
            rightBackDrive.setPower(rightBackPower);
        }

        telemetry.addData("Status", "Run Time: " + runtime);
        telemetry.addData("Front left/Right", "%4.2f, %4.4f", leftFrontPower, rightFrontPower);
        telemetry.addData("Back  left/Right", "%4.2f, %4.4f", leftBackPower, rightBackPower);
        telemetry.addData("Back Encoder l/r", "%d, %d", positions[0], positions[1]);
        telemetry.addData("Front Encoder l/r", "%d, %d", positions[2], positions[3]);
    }


    /// Run actions based on buttons pressed on Manipulator's controller
    void doActions(){
        if (Qol.checkButton(gamepad1.a, "a")){
            if (isToggled("horiz") || gamepad1.right_trigger > 0.5){
                Extend_Hori(!isToggled("horiz"));
            } else {
                ManualExtend();
            }
        }
        if (Qol.checkButton(gamepad1.b, "b"))
            Clamp(!isToggled("clamp"));

        if (Qol.checkButton(gamepad1.x, "x"))
            Extend_Vert(!isToggled("vert"));

        if (Qol.checkButton(gamepad1.y, "y"))
            Flip(!isToggled("flip"));

        if (Qol.checkButton(gamepad2.a, "a2"))
            Speed();
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
        telemetry.addData("Clamp", isToggled("clamp")? "Clamped":"Open");
        telemetry.addData("Lift", isToggled("vert")? "Up":"Down");
        telemetry.addData("Flipper", isToggled("flip")? "Flipped": "Not Flipped");
    }

    void Speed(){
        double leftFrontPower  = -0.5 + 1;
        double rightFrontPower = -0.5 - 1;
        double leftBackPower   = -0.5 - 1;
        double rightBackPower  = -0.5 + 1;

        double max;


        // Normalize the values so no wheel power exceeds 100%
        // This ensures that the robot maintains the desired motion.
        max = Math.max(Math.abs(leftFrontPower), Math.abs(rightFrontPower));
        max = Math.max(max, Math.abs(leftBackPower));
        max = Math.max(max, Math.abs(rightBackPower));

        // Allow speed to be slowed based on pressure put on the left trigger
        //max *= 3-(gamepad2.left_trigger*2);

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

        speeding = true;
        //registerCheckingCallback(() -> );
    }
}
