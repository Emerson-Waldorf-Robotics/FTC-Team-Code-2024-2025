package org.firstinspires.ftc.teamcode;

import static org.firstinspires.ftc.teamcode.shared.Shared.*;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;


@Autonomous(name="Auto Dance Mode", group = "Auto")
public class AutoDance extends LinearOpMode
{

    public boolean getOpActive(){
        return opModeIsActive();
    }


    @Override public void runOpMode()
    {
        hardwareInit(hardwareMap, telemetry, this::getOpActive);

        telemetry.addData(">", "Touch START to start OpMode");
        telemetry.update();

        waitForStart();

        spin(true);

        while (opModeIsActive()){
            runCallbacks();
        }
    }

    public void spin(boolean left){
        double leftFrontPower  = left?1:-1;
        double rightFrontPower = left?-1:1;
        double leftBackPower   = left?1:-1;
        double rightBackPower  = left?-1:1;


        // Send calculated power to wheels
        leftFrontDrive.setPower(leftFrontPower);
        rightFrontDrive.setPower(rightFrontPower);
        leftBackDrive.setPower(leftBackPower);
        rightBackDrive.setPower(rightBackPower);

        registerCallback(() -> spin(!left), 1000);
    }
}
