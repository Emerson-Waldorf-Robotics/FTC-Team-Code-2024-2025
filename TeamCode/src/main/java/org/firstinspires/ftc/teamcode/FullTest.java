package org.firstinspires.ftc.teamcode;

import static android.system.Os.kill;

import androidx.annotation.NonNull;

import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;
import com.qualcomm.robotcore.hardware.Servo;

import java.util.HashMap;
import java.util.Map;


@TeleOp(name="Cross Your Fingers", group = "AAAAAA")
public class FullTest extends LinearOpMode
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

        int EXTEND_DIFFERENCE = 500;
        int VERTICAL_DIFFERENCE = 1720;
        int PIVOT_DISTANCE = 0;

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
        clamp.scaleRange(0.7, 0.85);
        flip  = hardwareMap.get(Servo.class, "flipper_servo");
        flip.scaleRange(.2, 1);

        //telemetry.addData(">", "Touch START to Initialize.");
        //telemetry.update();
        //waitForStart();

        // Pivot reset position
        pivot.setPosition(0.82);

        telemetry.addData(">", "Touch START to Start OpMode.");
        telemetry.update();

        waitForStart();

        pivot.setPosition(0.82);

        while (opModeIsActive())
        {

            if (Qol.checkButton(gamepad1.a, "a")){
                telemetry.addData("QOL", "Was pressed");

                if (!isTrue(action1.get("a"))) {
                    telemetry.addData("A", "Going through");

                    action1.put("a", true);

                    // Open clamp
                    clamp.setPosition(1);

                    // Pivot down
                    pivot.setPosition(0);
                    // Extend
                    MoveMotor(EXTEND_DIFFERENCE, extend_horiz, true, 2000);
                } else {
                    telemetry.addData("A", "Not going through");

                    action1.put("a", false);

                    if (isTrue(action1.get("x"))){
                        // Bring bucket down
                        MoveMotor(0, extend_vert, true, 1000);
                    }

                    // Pivot up
                    pivot.setPosition(1);
                    // Retract
                    MoveMotor(0, extend_horiz, true, 2000);

                    // Delay
                    sleep(750);

                    // Unclamp
                    clamp.setPosition(1);
                }
            }
            if (Qol.checkButton(gamepad1.b, "b")){
                if (!isTrue(action1.get("b"))) {
                    action1.put("b", true);
                    // Clamp
                    clamp.setPosition(0);
                } else {
                    action1.put("b", false);
                    // Unclamp
                    clamp.setPosition(1);
                }
            }
            if (Qol.checkButton(gamepad1.x, "x")){
                if (!isTrue(action1.get("x"))) {
                    action1.put("x", true);

//                    // Safeties:
//                    // Are we currently horizontally retracted?
//                    if (!isTrue(action1.get("a"))){
//                        // Unclamp in case driver forgot to
//                        clamp.setPosition(1);
//
//                        sleep(10);
//
//                        // Move the Clamp out of the way
//                        pivot.setPosition(0.82);
//
//                        sleep(10);
//                    }

                    // Extend up
                    MoveMotor(VERTICAL_DIFFERENCE, extend_vert, true, 3500);

                    sleep(200);
                } else {
                    action1.put("x", false);

                    // Safety unflip during lower
                    flip.setPosition(0);

                    if (!isTrue(action1.get("a"))){
                        // Move the Clamp out of the way
                        pivot.setPosition(0.82);
                    }

                    // Down again
                    MoveMotor(0, extend_vert, true, 1000);
                }
            }
            if (Qol.checkButton(gamepad1.y, "y")){
                if (!isTrue(action1.get("y"))) {

                    // Don't flip while lowered
                    if (isTrue(action1.get("x"))){
                        action1.put("y", true);
                        // Flip
                        flip.setPosition(1);
                    } else {
                        telemetry.addLine("Wont flip while lowered.");
                    }
                } else {
                    action1.put("y", false);
                    // Unflip
                    flip.setPosition(0);
                }
            }


            telemetry.update();

            sleep(10);
        }
    }
}
