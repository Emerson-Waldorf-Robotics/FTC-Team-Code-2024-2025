package org.firstinspires.ftc.teamcode;

import androidx.annotation.NonNull;

import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;
import com.qualcomm.robotcore.hardware.Servo;

import java.util.HashMap;


@TeleOp(name="Servo Test", group = "Tests")
public class ServoTest extends LinearOpMode
{
    private Servo pivot = null;


    @Override public void runOpMode()
    {

        int EXTEND_DIFFERENCE = 500;
        int VERTICAL_DIFFERENCE = 1720;
        int PIVOT_DISTANCE = 0;


        pivot = hardwareMap.get(Servo.class, "flipper_servo");
        pivot.scaleRange(0.16, 1);
        //pivot.setDirection(Servo.Direction.REVERSE);
        //pivot.setDirection(Servo.Direction.REVERSE);

        telemetry.addData(">", "Touch START to start OpMode");
        telemetry.update();

        waitForStart();

        while (opModeIsActive()){
            for (float i = 0.0f; i < 1; i += 0.005f){
                if (!opModeIsActive()){
                    break;
                }
                pivot.setPosition(i);
                telemetry.addData("Position", "%f", i);
                telemetry.update();

                sleep(100);
            }
        }
    }
}
