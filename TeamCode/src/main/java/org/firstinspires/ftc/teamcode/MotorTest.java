package org.firstinspires.ftc.teamcode;
import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;

@TeleOp(name="Motor Test Manual", group = "Tests")
public class MotorTest extends LinearOpMode
{
    private DcMotor motor = null;

    @Override public void runOpMode()
    {

        //int startpos;


        // Initialize the hardware variables. Note that the strings used here as parameters
        // to 'get' must match the names assigned during the robot configuration.
        // step (using the FTC Robot Controller app on the phone).
        motor  =  hardwareMap.get(DcMotorEx.class, "extend_horizontal");
        motor.setMode(DcMotorEx.RunMode.STOP_AND_RESET_ENCODER);

        telemetry.addData(">", "Touch START to start OpMode");
        telemetry.update();
        waitForStart();

        motor.setMode(DcMotorEx.RunMode.RUN_USING_ENCODER);

        while (opModeIsActive())
        {

            motor.setPower(gamepad1.left_stick_y/(gamepad1.a? 1 : 2));

            // Tell the driver what we see, and what to do.
            telemetry.addData("Motor Position", "%d", motor.getCurrentPosition());
            telemetry.addData("Motor Power", "%f", motor.getPower());
            telemetry.update();

            sleep(10);
        }
    }
}
