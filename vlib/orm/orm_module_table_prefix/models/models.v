module models

pub struct SendSMSRequest {
pub mut:
	id     int @[primary; sql: serial]
	msisdn string
	msg    string
	sender string
}
