package org.firstinspires.ftc.teamcode;

import static org.firstinspires.ftc.teamcode.shared.Shared.*;

import static java.lang.Thread.sleep;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.OpMode;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.hardware.TouchSensor;

import org.firstinspires.ftc.robotcore.external.hardware.camera.WebcamName;
import org.firstinspires.ftc.robotcore.external.hardware.camera.controls.ExposureControl;
import org.firstinspires.ftc.robotcore.external.hardware.camera.controls.GainControl;
import org.firstinspires.ftc.vision.VisionPortal;
import org.firstinspires.ftc.vision.apriltag.AprilTagDetection;
import org.firstinspires.ftc.vision.apriltag.AprilTagProcessor;

import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;

@Autonomous(name = "Auto OpMode", group = "Auto")
public class RunAuto extends OpMode {

    private static final int DESIRED_TAG_ID = -1;     // Choose the tag you want to approach or set to -1 for ANY tag.
    private VisionPortal visionPortal;               // Used to manage the video source.
    private AprilTagProcessor aprilTag;              // Used for managing the AprilTag detection process.
    private AprilTagDetection desiredTag = null;     // Used to hold the data for a detected AprilTag

    void forward(int howmuch, double vel){
        int leftFrontCurr = leftFrontDrive.getCurrentPosition();
        int leftBackCurr = leftBackDrive.getCurrentPosition();
        int rightFrontCurr = rightFrontDrive.getCurrentPosition();
        int rightBackCurr = rightBackDrive.getCurrentPosition();


        leftFrontDrive.setTargetPosition(leftFrontCurr + howmuch);
        leftBackDrive.setTargetPosition(leftBackCurr + howmuch);
        rightFrontDrive.setTargetPosition(rightFrontCurr + howmuch);
        rightBackDrive.setTargetPosition(rightBackCurr + howmuch);

        leftFrontDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        leftBackDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        rightBackDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        rightFrontDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);

        leftFrontDrive.setVelocity(vel);
        leftBackDrive.setVelocity(vel);
        rightBackDrive.setVelocity(vel);
        rightFrontDrive.setVelocity(vel);
    }

    void backwards(int howmuch, double vel){
        forward(-howmuch, vel);
    }

    void turnRight(int degrees, double vel){
        turnLeft(-degrees, vel);
    }

    void turnLeft(int degrees, double vel){
        int leftFrontCurr = leftFrontDrive.getCurrentPosition();
        int leftBackCurr = leftBackDrive.getCurrentPosition();
        int rightFrontCurr = rightFrontDrive.getCurrentPosition();
        int rightBackCurr = rightBackDrive.getCurrentPosition();

        leftFrontDrive.setTargetPosition(degrees + leftFrontCurr);
        leftBackDrive.setTargetPosition(degrees + leftBackCurr);
        rightFrontDrive.setTargetPosition(-degrees + rightFrontCurr);
        rightBackDrive.setTargetPosition(-degrees + rightBackCurr);

        leftFrontDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        leftBackDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        rightBackDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        rightFrontDrive.setMode(DcMotor.RunMode.RUN_TO_POSITION);

        leftFrontDrive.setVelocity(vel);
        leftBackDrive.setVelocity(vel);
        rightBackDrive.setVelocity(vel);
        rightFrontDrive.setVelocity(vel);

    }


    // TODO: Calibrate this
    private static final int LOW_BUCKET_VERTICAL = 224;

    private void setManualExposure(int exposureMS, int gain) throws InterruptedException {
        // Wait for the camera to be open, then use the controls

        if (visionPortal == null) {
            return;
        }

        // Make sure camera is streaming before we try to set the exposure controls
        if (visionPortal.getCameraState() != VisionPortal.CameraState.STREAMING) {
            telemetry.addData("Camera", "Waiting");
            telemetry.update();
            while ((visionPortal.getCameraState() != VisionPortal.CameraState.STREAMING)) {
                sleep(20);
            }
            telemetry.addData("Camera", "Ready");
            telemetry.update();
        }

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


    void addNeededTelemetry() {
        telemetry.addData("Linear extension:", touch.isPressed() ? "In" : "Out");
        telemetry.addData("Clamp", isToggled("clamp")? "Clamped":"Open");
        telemetry.addData("Lift", isToggled("vert")? "Up":"Down");
        telemetry.addData("Flipper", isToggled("flip")? "Flipped": "Not Flipped");
    }

    @Override
    public void init() {
        try {
            setManualExposure(6, 250);  // Use low exposure time to reduce motion blur
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }

        hardwareInit(hardwareMap, telemetry, () -> true);

        // Wait for the game to start (driver presses START)
        telemetry.addData("Status", "Initialized");
        telemetry.update();
    }

    @Override
    public void loop() {
        runCallbacks();
        addNeededTelemetry();
        telemetry.update();
    }

    @Override
    public void start() {
        runtime.reset();

        initMotors();

        backwards(500, 500);
        turnLeft(500, 500);

        Extend_Vert(true);

        // Flip when at max height
        registerCheckingCallback(() -> Flip(true), () -> (extend_vert.getCurrentPosition() == VERTICAL_DIFFERENCE));
    }
}
