package org.firstinspires.ftc.teamcode;

import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;

@TeleOp(name="Do Go Thingy", group = "Tests")
// Testing RUN_TO_POSITION
public class RunToPositionTest extends LinearOpMode
{
    private DcMotorEx motor = null;

    void MoveMotor(int where){
        int spos = motor.getCurrentPosition();
        int epos = spos + where;
        telemetry.addLine("Running motor...");
        telemetry.addLine("--------------------------------------------------");
        telemetry.addData("Moving by", "%d", where);
        telemetry.addData("Start position", "%d", spos);
        telemetry.addData("End Position", "%d", epos);
        telemetry.update();

        motor.setTargetPosition(epos);
        motor.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        motor.setVelocity(2000);
    }

    @Override public void runOpMode()
    {

        //int startpos;

        // Last button position
        boolean lpos = false;
        // Which thing to do
        boolean dothing = false;

        // Initialize the hardware variables. Note that the strings used here as parameters
        // to 'get' must match the names assigned during the robot configuration.
        // step (using the FTC Robot Controller app on the phone).
        motor  =  hardwareMap.get(DcMotorEx.class, "extend_motor");

        telemetry.addData(">", "Touch START to start OpMode");
        telemetry.update();
        waitForStart();

        while (opModeIsActive())
        {

            if (gamepad1.a && !lpos){
                lpos = true;
                if (dothing) {
                    dothing = false;
                    MoveMotor(360);
                } else {
                    dothing = true;
                    MoveMotor(-360);
                }
            }
            if (!gamepad1.a) {
                lpos = false;
            }

            // Tell the driver what we see, and what to do.
            telemetry.addData("Motor Position", "%d", motor.getCurrentPosition());
            telemetry.addData("Motor Power", "%f", motor.getPower());
            telemetry.update();

            sleep(10);
        }
    }
}
