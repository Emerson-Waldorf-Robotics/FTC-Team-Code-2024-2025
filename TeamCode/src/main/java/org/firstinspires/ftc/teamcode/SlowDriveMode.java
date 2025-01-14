package org.firstinspires.ftc.teamcode;

import com.qualcomm.robotcore.eventloop.opmode.LinearOpMode;
import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.util.ElapsedTime;


@TeleOp(name="Slow Drive Mode", group="Linear OpMode")
public class SlowDriveMode extends LinearOpMode {

    // Declare OpMode members for each of the 4 motors.
    private ElapsedTime runtime = new ElapsedTime();
    private DcMotor leftFrontDrive = null;
    private DcMotor leftBackDrive = null;
    private DcMotor rightFrontDrive = null;
    private DcMotor rightBackDrive = null;
    private DcMotor extendMotor = null;

    @Override
    public void runOpMode() {

        // Initialize the hardware variables. Note that the strings used here must correspond
        // to the names assigned during the robot configuration step on the DS or RC devices.
        leftFrontDrive  = hardwareMap.get(DcMotor.class, "left_front");
        leftBackDrive  = hardwareMap.get(DcMotor.class, "left_back");
        rightFrontDrive = hardwareMap.get(DcMotor.class, "right_front");
        rightBackDrive = hardwareMap.get(DcMotor.class, "right_back");
        //extendMotor = hardwareMap.get(DcMotor.class, "extend_motor");

        int[] startPositions = {
                leftBackDrive.getCurrentPosition(),
                rightBackDrive.getCurrentPosition(),
                leftFrontDrive.getCurrentPosition(),
                rightFrontDrive.getCurrentPosition(),
                //extendMotor.getCurrentPosition()
        };

        leftFrontDrive.setDirection(DcMotor.Direction.FORWARD);
        leftBackDrive.setDirection(DcMotor.Direction.FORWARD);
        rightFrontDrive.setDirection(DcMotor.Direction.REVERSE);
        rightBackDrive.setDirection(DcMotor.Direction.REVERSE);


        //DcMotor.RunMode.

        // Wait for the game to start (driver presses START)
        telemetry.addData("Status", "Initialized");
        telemetry.addData("Mode", "%s", leftFrontDrive.getMode().toString());
        telemetry.update();

        waitForStart();
        runtime.reset();

        // run until the end of the match (driver presses STOP)
        while (opModeIsActive()) {
            int[] positions = {
                    leftBackDrive.getCurrentPosition() - startPositions[0],
                    rightBackDrive.getCurrentPosition() - startPositions[1],
                    leftFrontDrive.getCurrentPosition() - startPositions[2],
                    rightFrontDrive.getCurrentPosition() - startPositions[3]
            };

            double max;

            // POV Mode uses left joystick to go forward & strafe, and right joystick to rotate.
            double axial   = -gamepad1.left_stick_y;  // Note: pushing stick forward gives negative value
            double lateral =  gamepad1.left_stick_x;
            double yaw     =  gamepad1.right_stick_x;

            // Combine the joystick requests for each axis-motion to determine each wheel's power.
            // Set up a variable for each drive wheel to save the power level for telemetry.

            /*
            double leftFrontPower  = (axial + lateral + yaw);
            double rightFrontPower = axial - lateral - yaw;
            double leftBackPower   = (axial - lateral + yaw);
            double rightBackPower  = axial + lateral - yaw;
            */
            double leftFrontPower  = axial + lateral + yaw;
            double rightFrontPower = axial - lateral - yaw;
            double leftBackPower   = axial + lateral + yaw;
            double rightBackPower  = axial - lateral - yaw;

            // double extendMotorPower = axial + lateral + yaw;


            // Normalize the values so no wheel power exceeds 100%
            // This ensures that the robot maintains the desired motion.
            max = Math.max(Math.abs(leftFrontPower), Math.abs(rightFrontPower));
            max = Math.max(max, Math.abs(leftBackPower));
            max = Math.max(max, Math.abs(rightBackPower));
            // max = Math.max(max, Math.abs(extendMotorPower));
            max *= 1;

            if (max > 1.0) {
                leftFrontPower  /= max;
                rightFrontPower /= max;
                leftBackPower   /= max;
                rightBackPower  /= max;

                // extendMotorPower /= max;
            }

            // This is test code:
            //
            // Uncomment the following code to test your motor directions.
            // Each button should make the corresponding motor run FORWARD.
            //   1) First get all the motors to take to correct positions on the robot
            //      by adjusting your Robot Configuration if necessary.
            //   2) Then make sure they run in the correct direction by modifying the
            //      the setDirection() calls above.
            // Once the correct motors move in the correct direction re-comment this code.

            /*
            leftFrontPower  = gamepad1.x ? 1.0 : 0.0;  // X gamepad
            leftBackPower   = gamepad1.a ? 1.0 : 0.0;  // A gamepad
            rightFrontPower = gamepad1.y ? 1.0 : 0.0;  // Y gamepad
            rightBackPower  = gamepad1.b ? 1.0 : 0.0;  // B gamepad
            */

            // Send calculated power to wheels
            leftFrontDrive.setPower(leftFrontPower);
            rightFrontDrive.setPower(rightFrontPower);
            leftBackDrive.setPower(leftBackPower);
            rightBackDrive.setPower(rightBackPower);


            if (gamepad2.dpad_up) {
                extendMotor.setPower(0.5);
            }
            if (gamepad2.dpad_down) {
                extendMotor.setPower(-0.5);
            }
            else {
                extendMotor.setPower(0);
            }
            

            // Show the elapsed game time and wheel power.
            telemetry.addData("Status", "Run Time: " + runtime.toString());
            telemetry.addData("Front left/Right", "%4.2f, %4.4f", leftFrontPower, rightFrontPower);
            telemetry.addData("Back  left/Right", "%4.2f, %4.4f", leftBackPower, rightBackPower);
            telemetry.addData("Back Encoder l/r", "%d, %d", positions[0], positions[1]);
            telemetry.addData("Front Encoder l/r", "%d, %d", positions[2], positions[3]);
            telemetry.update();
        }
    }}
