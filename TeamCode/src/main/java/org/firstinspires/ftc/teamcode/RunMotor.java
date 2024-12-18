package org.firstinspires.ftc.teamcode;

import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;

@TeleOp(name="Motor Test 2", group = "Tests")
public class RunMotor extends LinearOpMode
{
    private DcMotor motor = null;
    double moved = 0;
    int degrees = 20;

    void deg30(double power){
        int spos = motor.getCurrentPosition();
        int cpos;
        int last = 0;

        motor.setPower(power);
        while (opModeIsActive()){
            cpos = motor.getCurrentPosition();
            //if (Math.abs(cpos - last) < 1){
            //    break;
            //}
            moved = Math.abs(cpos - spos);
            if (moved > degrees){
                break;
            }

            telemetry.addData("Moved", "%f/%d", moved, degrees);
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
        motor  =  hardwareMap.get(DcMotor.class, "test_motor");

        motor.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        float speed = 0;
        // Last button states
        boolean lb = false, lb2 = false;

        float motorSpeed = 1f;

        telemetry.addData(">", "Touch START to start OpMode");
        telemetry.update();
        waitForStart();

        while (opModeIsActive()) {
            {
                if (gamepad1.a && !lb2) {
                    lb2 = true;
                    deg30(-motorSpeed);
                    continue;
                }
                if (!gamepad1.a) {
                    lb2 = false;
                }
            }
            {
                if (gamepad1.b && !lb) {
                    lb = true;
                    deg30(motorSpeed);
                    continue;
                }
                if (!gamepad1.a) {
                    lb = false;
                }
            }

            // Tell the driver what we see, and what to do.
            telemetry.addData("Motor Position", "%d", motor.getCurrentPosition());
            telemetry.addData("Motor Power", "%f", motor.getPower());
            telemetry.update();

            sleep(10);
        }
    }
}
