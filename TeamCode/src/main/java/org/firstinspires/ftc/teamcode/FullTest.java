package org.firstinspires.ftc.teamcode;

import androidx.annotation.NonNull;

import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;
import com.qualcomm.robotcore.hardware.Servo;

import java.util.HashMap;



@TeleOp(name="Cross Your Fingers", group = "AAAAAA")
public class FullTest extends LinearOpMode
{
    static class Qol {
        // Last button position
        static HashMap<String, Boolean> buttonStates = new HashMap<>(4);

        // Which thing to do
        static boolean dothing = false;

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

    HashMap<String, Boolean> action1 = new HashMap<>(4);

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


    void MoveMotor(int where, @NonNull DcMotorEx motor, boolean exact){
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
        // SPEEEEEED
        motor.setVelocity(2000);
    }

    // Move the motor until too much resistance
    void MoveMotorUntilResist(int where, @NonNull Servo motor){
        motor.setPosition(where);

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
        pivot.scaleRange(0, .6);
        clamp = hardwareMap.get(Servo.class, "clamp_servo");
        clamp.scaleRange(0, 0.4);
        flip  = hardwareMap.get(Servo.class, "flipper_servo");
        flip.scaleRange(0.2, 1);

        telemetry.addData(">", "Touch START to start OpMode");
        telemetry.update();
        waitForStart();

        while (opModeIsActive())
        {

            if (Qol.checkButton(gamepad1.a, "a")){
                if (isTrue(action1.get("a"))) {
                    action1.put("a", false);
                    // Linear extension
                    MoveMotor(EXTEND_DIFFERENCE, extend_horiz, true);
                    // Pivot clamp arm
                    pivot.setPosition(1);
                } else {
                    action1.put("a", true);
                    MoveMotor(0, extend_horiz, true);
                    pivot.setPosition(0);
                }
            }

            if (Qol.checkButton(gamepad1.b, "b")){
                if (isTrue(action1.get("b"))) {
                    action1.put("b", false);
                    clamp.setPosition(1);
                } else {
                    action1.put("b", true);
                    clamp.setPosition(0);
                }
            }

            if (Qol.checkButton(gamepad1.x, "x")){
                if (isTrue(action1.get("x"))) {
                    action1.put("x", false);
                    MoveMotor(VERTICAL_DIFFERENCE, extend_vert, true);
                } else {
                    action1.put("x", true);
                    MoveMotor(0, extend_vert, true);
                }
            }

            if (Qol.checkButton(gamepad1.y, "y")){
                if (isTrue(action1.get("y"))) {
                    action1.put("y", false);
                    flip.setPosition(1);
                } else {
                    action1.put("y", true);
                    flip.setPosition(0);
                }
            }

            // Tell the driver what we see, and what to do.
            telemetry.addData("Motor Position", "%f", flip.getPosition());
            telemetry.update();

            sleep(10);
        }
    }
}
