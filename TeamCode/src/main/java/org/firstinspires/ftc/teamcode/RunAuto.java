package org.firstinspires.ftc.teamcode;

import static org.firstinspires.ftc.teamcode.shared.Shared.*;

import static java.lang.Thread.sleep;

import com.qualcomm.hardware.dfrobot.HuskyLens;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.OpMode;
import com.qualcomm.robotcore.hardware.DcMotor;

import org.firstinspires.ftc.robotcore.external.hardware.camera.WebcamName;
import org.firstinspires.ftc.robotcore.external.hardware.camera.controls.ExposureControl;
import org.firstinspires.ftc.robotcore.external.hardware.camera.controls.GainControl;
import org.firstinspires.ftc.vision.VisionPortal;
import org.firstinspires.ftc.vision.apriltag.AprilTagDetection;
import org.firstinspires.ftc.vision.apriltag.AprilTagProcessor;

import java.util.Arrays;
import java.util.concurrent.TimeUnit;

@Autonomous(name = "Auto OpMode", group = "Auto")
public class RunAuto extends OpMode {

    private static final int DESIRED_TAG_ID = -1;     // Choose the tag you want to approach or set to -1 for ANY tag.
    private VisionPortal visionPortal;               // Used to manage the video source.
    private AprilTagProcessor aprilTag;              // Used for managing the AprilTag detection process.
    private AprilTagDetection desiredTag = null;     // Used to hold the data for a detected AprilTag


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

        //Camera.selectAlgorithm(HuskyLens.Algorithm.OBJECT_TRACKING);

        leftFrontDrive.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        leftBackDrive.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        rightFrontDrive.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        rightBackDrive.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);

        // Wait for the game to start (driver presses START)
        telemetry.addData("Status", "Ready");
        telemetry.addLine("Press START to Auto");
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

        backwards(450);

        //motoGO(500);
        //waitMoveDone();

        turnLeft(350);

        //motoGO(1000);
        //waitMoveDone();

        //strafeLeft(200);

        motoGO(1000);

        //try {
        //    sleep(50);
        //} catch (InterruptedException e) {
        //    throw new RuntimeException(e);
        //}

        Extend_Vert(true, true);

        // Flip when at max height
        registerCheckingCallback(() -> {
            Flip(true);
            registerCallback(this::step2, 500);
        }, () -> (extend_vert.getCurrentPosition() > VERTICAL_DIFFERENCE-20));
    }

    void step2(){
        turnLeft(350);
        forward(100);
        motoGO(1000);

        Flip(false);

        Extend_Vert(false, true);


        Extend_Hori(true);

        PivotPos(2);

        registerCallback(() -> {
            Clamp(true);
            // Clamp callback
            registerCallback(() -> {
                // After clamp
                Extend_Hori(false);
                // Extend Callback
                registerCheckingCallback(this::step3, () -> touch.isPressed());
            }, 250);
        }, 400);
    }

    void step3(){
        PivotPos(1);


        Clamp(false);

        turnRight(350);
        backwards(100);
        motoGO(1000);

        //telemetry.addLine(Arrays.toString(Camera.blocks(1)));
        telemetry.update();
        while(true){}
    }
}
