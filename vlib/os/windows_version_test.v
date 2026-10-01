module os

fn test_windows_version_parts_accepts_localized_ver_output() {
	for output in ['\r\nMicrosoft Windows [Version 10.0.22631.4317]\r\n',
		'Microsoft Windows [versión 6.1.7601]', 'Microsoft Windows [版本 10.0.19045.0]'] {
		release, build := windows_version_parts(output)
		assert release in ['10.0', '6.1']
		assert build in ['22631', '7601', '19045']
	}
}

fn test_windows_version_parts_degrades_on_missing_or_invalid_version() {
	for output in ['', 'exec failed (SetHandleInformation): The handle is invalid.', 'Microsoft Windows',
		'10', '10.0', '10.0.', '10.x.22631', '-10.0.22631'] {
		release, build := windows_version_parts(output)
		assert release == ''
		assert build == ''
	}
}
