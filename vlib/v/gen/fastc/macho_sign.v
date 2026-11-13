module fastc

import v.util.machosign

// FastcCodesignShim owns the temporary codesign shim and its previous PATH value.
pub type FastcCodesignShim = machosign.CodesignShim

// fastc_codesign_shim_dir prepares the shared Mach-O signer's temporary codesign shim.
pub fn fastc_codesign_shim_dir() FastcCodesignShim {
	return FastcCodesignShim(machosign.codesign_shim_dir())
}

// fastc_remove_codesign_shim_dir restores PATH and removes the temporary codesign shim.
pub fn fastc_remove_codesign_shim_dir(shim FastcCodesignShim) {
	machosign.remove_codesign_shim_dir(machosign.CodesignShim(shim))
}

// fastc_sign_macho_adhoc gives the executable at path an ad-hoc Mach-O signature.
pub fn fastc_sign_macho_adhoc(path string) ! {
	machosign.sign_adhoc(path)!
}
