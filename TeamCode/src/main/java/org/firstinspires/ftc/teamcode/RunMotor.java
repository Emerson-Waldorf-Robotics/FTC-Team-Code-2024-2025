package org.firstinspires.ftc.teamcode;

import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;

@TeleOp(name="Motor Test Button", group = "Tests")
public class RunMotor extends LinearOpMode
{
    private DcMotor motor = null;
    double moved = 0;

    void extend(double speed){
        motor.setTargetPosition(600);
        motor.setMode(DcMotor.RunMode.RUN_TO_POSITION);
    }

    /** @noinspection SameParameterValue*/
    void deg(double power, int move){
        telemetry.addData("Moving in seconds", 3);
        telemetry.update();
        sleep(3000);

        int spos = motor.getCurrentPosition();
        int cpos;
        int last = 0;

        motor.setPower(power);
        while (opModeIsActive()){
            cpos = motor.getCurrentPosition();
            if (Math.abs(cpos - last) < 2){
                //exit(1);
                //telemetry.addData("Crash", "%f", 0);
                //telemetry.update();
                //break;
            }
            moved = Math.abs(cpos - spos);
            if (moved > move){
                break;
            }

            telemetry.addData("Moved", "%f/%d", moved, move);
            telemetry.addData("Motor Power", "%f", motor.getPower());
            telemetry.addData("Motor Supposed to be at power", "%f", power);
            telemetry.update();
            sleep(20);
        }
        motor.setPower(0);
    }

    @Override public void runOpMode()
    {

        //int startpos;


        // Initialize the hardware variables. Note that the strings used here as parameters
        // to 'get' must match the names assigned during the robot configuration.
        // step (using the FTC Robot Controller app on the phone).
        motor  =  hardwareMap.get(DcMotor.class, "extend_motor");

        motor.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        while (motor.getCurrentPosition() != 0){
            telemetry.addData("Waiting for motor reset", "...");
            telemetry.update();
            sleep(20);
            idle();
        }
        // Last button states
        boolean lb = false, lb2 = false;

        float motorSpeed = 0.75f;

        telemetry.addData(">", "Touch START to start OpMode");
        telemetry.update();
        waitForStart();

        while (opModeIsActive()) {
            {
                if (gamepad1.a && !lb2) {
                    lb2 = true;
                    deg(-motorSpeed, 600);
                    continue;
                }
                if (!gamepad1.a) {
                    lb2 = false;
                }
            }
            {
                if (gamepad1.b && !lb) {
                    lb = true;
                    deg(motorSpeed, 600);
                    continue;
                }
                if (!gamepad1.b) {
                    lb = false;
                }
            }

            // Tell the driver what we see, and what to do.
            telemetry.addData("Motor Position", "%d", motor.getCurrentPosition());
            telemetry.addData("Motor Power", "%f", motor.getPower());
            telemetry.addData("Motor Supposed to be at power", "%d", 0);
            telemetry.addData("Lb1", lb);
            telemetry.addData("Lb2", lb2);
            telemetry.update();

            sleep(10);
        }
    }
}
