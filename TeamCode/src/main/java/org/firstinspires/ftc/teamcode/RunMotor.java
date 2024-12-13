package org.firstinspires.ftc.teamcode;

import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;

@TeleOp(name="Motor Test 2", group = "Tests")
public class RunMotor extends LinearOpMode
{
    private DcMotor motor = null;
    double moved = 0;

    void deg30(double power){
        int cpos = motor.getCurrentPosition();
        motor.setPower(power);
        while (opModeIsActive()){
            if (Math.abs(motor.getCurrentPosition() - cpos) > 30){
                break;
            }
        }
        motor.setPower(0);
        moved = Math.abs(motor.getCurrentPosition() - cpos);
    }

    @Override public void runOpMode()
    {

        //int startpos;


        // Initialize the hardware variables. Note that the strings used here as parameters
        // to 'get' must match the names assigned during the robot configuration.
        // step (using the FTC Robot Controller app on the phone).
        motor  =  hardwareMap.get(DcMotor.class, "test_motor");
        int spos = motor.getCurrentPosition();
        float speed = 0;
        // Last button states
        boolean lb = false, lb2 = false;
        boolean running = false;

        float motorSpeed = 0.6f;

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
            telemetry.addData("Moved", moved);
            telemetry.update();

            sleep(10);
        }
    }
}
