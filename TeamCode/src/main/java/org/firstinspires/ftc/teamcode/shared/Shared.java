package org.firstinspires.ftc.teamcode.shared;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import androidx.annotation.NonNull;

import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;

public class Shared {
    public static void MoveMotor(int where, @NonNull DcMotorEx motor, boolean exact, int vel, Telemetry telemetry){
        telemetry.addLine("Running motor...");
        telemetry.addLine("--------------------------------------------------");
        telemetry.addData("Moving by", "%d", where);

        if (exact){
            telemetry.update();

            motor.setTargetPosition(where);
        } else {
            int spos = motor.getCurrentPosition();
            int epos = spos + where;
            telemetry.addData("Start position", "%d", spos);
            telemetry.addData("End Position", "%d", epos);
            telemetry.update();

            motor.setTargetPosition(epos);
        }

        motor.setMode(DcMotor.RunMode.RUN_TO_POSITION);
        motor.setVelocity(vel);
    }
}
