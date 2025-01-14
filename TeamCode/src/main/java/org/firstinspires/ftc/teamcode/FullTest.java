package org.firstinspires.ftc.teamcode;

import static android.system.Os.kill;

import androidx.annotation.NonNull;

import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.ElapsedTime;

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
        clamp.scaleRange(0.7, 0.85);
        flip  = hardwareMap.get(Servo.class, "flipper_servo");
        flip.scaleRange(.2, 1);

        //telemetry.addData(">", "Touch START to Initialize.");
        //telemetry.update();
        //waitForStart();

        // Pivot reset position
        ResetPivot();

        telemetry.addData(">", "Touch START to Start OpMode.");
        telemetry.update();

        waitForStart();
        runtime.reset();

        ResetPivot();

        while (opModeIsActive())
        {

            drive(startPositions);

            // TODO: Variable extension amount
            if (Qol.checkButton(gamepad1.a, "a")){
                telemetry.addData("QOL", "Was pressed");

                Extend_Hori(!isTrue(action1.get("a")));
            }
            if (Qol.checkButton(gamepad1.b, "b")){
                Clamp(!isTrue(action1.get("b")));
            }
            if (Qol.checkButton(gamepad1.x, "x")){
                Extend_Vert(!isTrue(action1.get("x")));
            }
            if (Qol.checkButton(gamepad1.y, "y")){
                Flip(!isTrue(action1.get("y")));
            }


            telemetry.update();

            sleep(10);
        }
    }

    void Extend_Vert(boolean up){
        if (up){
            action1.put("x", true);

            // Safeties:
            // Are we currently horizontally retracted?
            if (!isTrue(action1.get("a"))){
                // Unclamp in case driver forgot to
                Clamp(false);

                sleep(10);

                // Move the Clamp out of the way
                ResetPivot();

                sleep(10);
            }

            // Extend up
            MoveMotor(VERTICAL_DIFFERENCE, extend_vert, true, 5000);

            sleep(200);
        } else {
            action1.put("x", false);

            // Safety unflip during lower
            Flip(false);

            if (!isTrue(action1.get("a"))){
                // Move the Clamp out of the way
                ResetPivot();
            }

            // Down again
            MoveMotor(0, extend_vert, true, 1000);
        }
    }

    void Extend_Hori(boolean in){
        if (in){
            action1.put("a", true);

            // Open clamp
            Clamp(false);

            // Pivot down
            Pivot(false);
            // Extend
            MoveMotor(EXTEND_DIFFERENCE, extend_horiz, true, 2000);
        } else {
            action1.put("a", false);

            if (isTrue(action1.get("x"))){
                // Bring bucket down
                MoveMotor(0, extend_vert, true, 1000);
            }

            // Pivot up
            Pivot(true);
            // Retract
            MoveMotor(0, extend_horiz, true, 2000);

            // Delay
            sleep(750);

            // Unclamp
            Clamp(false);
        }
    }

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

    void Flip(boolean up){
        if (up){
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

    void ResetPivot(){
        pivot.setPosition(0.82);
    }

    void Pivot(boolean up){
        pivot.setPosition(up ? 1 : 0);
    }

    void drive(int[] startPositions){
        int[] positions = {
                leftBackDrive.getCurrentPosition() - startPositions[0],
                rightBackDrive.getCurrentPosition() - startPositions[1],
                leftFrontDrive.getCurrentPosition() - startPositions[2],
                rightFrontDrive.getCurrentPosition() - startPositions[3]
        };

        double max;

        double axial   = -gamepad1.left_stick_y;  // Note: pushing stick forward gives negative value
        double lateral =  gamepad1.left_stick_x;
        double yaw     =  gamepad1.right_stick_x;

        double leftFrontPower  = axial + lateral + yaw;
        double rightFrontPower = axial - lateral - yaw;
        double leftBackPower   = axial + lateral + yaw;
        double rightBackPower  = axial - lateral - yaw;


        // Normalize the values so no wheel power exceeds 100%
        // This ensures that the robot maintains the desired motion.
        max = Math.max(Math.abs(leftFrontPower), Math.abs(rightFrontPower));
        max = Math.max(max, Math.abs(leftBackPower));
        max = Math.max(max, Math.abs(rightBackPower));
        // max = Math.max(max, Math.abs(extendMotorPower));
        max *= 1;

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
}
