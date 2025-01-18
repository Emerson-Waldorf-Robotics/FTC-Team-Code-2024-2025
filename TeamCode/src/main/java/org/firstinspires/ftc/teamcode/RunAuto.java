package org.firstinspires.ftc.teamcode;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.ElapsedTime;

import org.firstinspires.ftc.robotcore.external.hardware.camera.BuiltinCameraDirection;
import org.firstinspires.ftc.robotcore.external.hardware.camera.WebcamName;
import org.firstinspires.ftc.robotcore.external.hardware.camera.controls.ExposureControl;
import org.firstinspires.ftc.robotcore.external.hardware.camera.controls.GainControl;
import org.firstinspires.ftc.vision.VisionPortal;
import org.firstinspires.ftc.vision.apriltag.AprilTagDetection;
import org.firstinspires.ftc.vision.apriltag.AprilTagProcessor;

import java.util.List;
import java.util.concurrent.TimeUnit;

@Autonomous(name = "Auto OpMode", group = "Auto")
public class RunAuto extends LinearOpMode {
    // Declare OpMode members for each of the 4 motors.
    private ElapsedTime runtime = new ElapsedTime();
    private DcMotorEx leftFrontDrive = null;
    private DcMotorEx leftBackDrive = null;
    private DcMotorEx rightFrontDrive = null;
    private DcMotorEx rightBackDrive = null;

    private static final int DESIRED_TAG_ID = -1;     // Choose the tag you want to approach or set to -1 for ANY tag.
    private VisionPortal visionPortal;               // Used to manage the video source.
    private AprilTagProcessor aprilTag;              // Used for managing the AprilTag detection process.
    private AprilTagDetection desiredTag = null;     // Used to hold the data for a detected AprilTag

    // Horizontal Extension Motor
    private DcMotorEx extend_horiz = null;
    // Vertical Extension Motor
    private DcMotorEx extend_vert  = null;
    private Servo pivot = null;
    private Servo clamp = null;
    private Servo flip  = null;

    void turnRight(int degrees){
        turnLeft(-degrees);
    }

    void forward(int howmuch){
        leftFrontDrive.setTargetPosition(howmuch);
        leftBackDrive.setTargetPosition(howmuch);
        rightFrontDrive.setTargetPosition(howmuch);
        rightBackDrive.setTargetPosition(howmuch);

        leftFrontDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        leftBackDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        rightBackDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        rightFrontDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);

        leftFrontDrive.setVelocity(3000);
        leftBackDrive.setVelocity(3000);
        rightBackDrive.setVelocity(3000);
        rightFrontDrive.setVelocity(3000);
    }

    void backwards(int howmuch){
        forward(-howmuch);
    }

    void turnLeft(int degrees){
        leftFrontDrive.setTargetPosition(degrees);
        leftBackDrive.setTargetPosition(degrees);
        rightFrontDrive.setTargetPosition(-degrees);
        rightBackDrive.setTargetPosition(-degrees);

        leftFrontDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        leftBackDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        rightBackDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        rightFrontDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);

        leftFrontDrive.setVelocity(3000);
        leftBackDrive.setVelocity(3000);
        rightBackDrive.setVelocity(3000);
        rightFrontDrive.setVelocity(3000);

    }

    @Override
    public void runOpMode() {
        setManualExposure(6, 250);  // Use low exposure time to reduce motion blur

        leftFrontDrive  = hardwareMap.get(DcMotorEx.class, "left_front");
        leftBackDrive  = hardwareMap.get(DcMotorEx.class, "left_back");
        rightFrontDrive = hardwareMap.get(DcMotorEx.class, "right_front");
        rightBackDrive = hardwareMap.get(DcMotorEx.class, "right_back");


        leftFrontDrive.setDirection(DcMotor.Direction.REVERSE);
        leftBackDrive.setDirection(DcMotor.Direction.REVERSE);
        rightFrontDrive.setDirection(DcMotor.Direction.FORWARD);
        rightBackDrive.setDirection(DcMotor.Direction.FORWARD);

        // Wait for the game to start (driver presses START)
        telemetry.addData("Status", "Initialized");
        telemetry.update();

        waitForStart();
        runtime.reset();

        boolean targetFound = false;
        desiredTag  = null;

        boolean onLeft;

        while (opModeInInit()){
            // Step through the list of detected tags and look for a matching tag
            List<AprilTagDetection> currentDetections = aprilTag.getDetections();
            for (AprilTagDetection detection : currentDetections) {
                // Look to see if we have size info on this tag.
                if (detection.metadata != null) {
                    telemetry.addData("We see", "Tag #%d", detection.id);
                    telemetry.update();
                    //switch (detection.id){
                    //    case
                    //}
                }
            }
        }

        // run until the end of the match (driver presses STOP)
        while (opModeIsActive()) {
            turnRight(90);
            sleep(500);
            forward(90);
            sleep(500);
        }
    }
    private void    setManualExposure(int exposureMS, int gain) {
        // Wait for the camera to be open, then use the controls

        if (visionPortal == null) {
            return;
        }

        // Make sure camera is streaming before we try to set the exposure controls
        if (visionPortal.getCameraState() != VisionPortal.CameraState.STREAMING) {
            telemetry.addData("Camera", "Waiting");
            telemetry.update();
            while (!isStopRequested() && (visionPortal.getCameraState() != VisionPortal.CameraState.STREAMING)) {
                sleep(20);
            }
            telemetry.addData("Camera", "Ready");
            telemetry.update();
        }

        // Set camera controls unless we are stopping.
        if (!isStopRequested())
        {
            ExposureControl exposureControl = visionPortal.getCameraControl(ExposureControl.class);
            if (exposureControl.getMode() != ExposureControl.Mode.Manual) {
                exposureControl.setMode(ExposureControl.Mode.Manual);
                sleep(50);
            }
            exposureControl.setExposure(exposureMS, TimeUnit.MILLISECONDS);
            sleep(20);
            GainControl gainControl = visionPortal.getCameraControl(GainControl.class);
            gainControl.setGain(gain);
            sleep(20);
        }
    }

    private void initAprilTag() {
        // Create the AprilTag processor by using a builder.
        aprilTag = new AprilTagProcessor.Builder().build();

        // Adjust Image Decimation to trade-off detection-range for detection-rate.
        // e.g. Some typical detection data using a Logitech C920 WebCam
        // Decimation = 1 ..  Detect 2" Tag from 10 feet away at 10 Frames per second
        // Decimation = 2 ..  Detect 2" Tag from 6  feet away at 22 Frames per second
        // Decimation = 3 ..  Detect 2" Tag from 4  feet away at 30 Frames Per Second
        // Decimation = 3 ..  Detect 5" Tag from 10 feet away at 30 Frames Per Second
        // Note: Decimation can be changed on-the-fly to adapt during a match.
        aprilTag.setDecimation(2);

        // Create the vision portal by using a builder.
        visionPortal = new VisionPortal.Builder()
                .setCamera(hardwareMap.get(WebcamName.class, "Webcam 1"))
                .addProcessor(aprilTag)
                .build();
    }

}
