package org.firstinspires.ftc.teamcode;

import com.acmerobotics.roadrunner.ftc.Encoder;
import com.acmerobotics.roadrunner.ftc.PositionVelocityPair;
import com.acmerobotics.roadrunner.ftc.RawEncoder;
import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;
import com.qualcomm.robotcore.util.Range;

import org.firstinspires.ftc.robotcore.external.hardware.camera.BuiltinCameraDirection;
import org.firstinspires.ftc.robotcore.external.hardware.camera.WebcamName;
import org.firstinspires.ftc.robotcore.external.hardware.camera.controls.ExposureControl;
import org.firstinspires.ftc.robotcore.external.hardware.camera.controls.GainControl;
import org.firstinspires.ftc.vision.VisionPortal;
import org.firstinspires.ftc.vision.apriltag.AprilTagDetection;
import org.firstinspires.ftc.vision.apriltag.AprilTagProcessor;

import java.util.List;
import java.util.concurrent.TimeUnit;

@TeleOp(name="Dead Wheel Test", group = "Tests")
public class DeadWheelTest extends LinearOpMode
{
    private Encoder deadwheel = null;

    @Override public void runOpMode()
    {

        int startpos;


        // Initialize the hardware variables. Note that the strings used here as parameters
        // to 'get' must match the names assigned during the robot configuration.
        // step (using the FTC Robot Controller app on the phone).
        deadwheel  =  new RawEncoder(hardwareMap.get(DcMotorEx.class, "deadwheel"));

        PositionVelocityPair deadwheeldata;

        startpos = deadwheel.getPositionAndVelocity().position;

        telemetry.addData(">", "Touch START to start OpMode");
        telemetry.update();
        waitForStart();

        while (opModeIsActive())
        {

            deadwheeldata = deadwheel.getPositionAndVelocity();

            // Tell the driver what we see, and what to do.
            telemetry.addData("Deadwheel Position", "%d", deadwheeldata.position - startpos);
            telemetry.addData("Deadwheel Velocity", "%d", deadwheeldata.velocity);
            telemetry.update();

            sleep(10);
        }
    }
}
