// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module consts

pub const (
	cgsm_speed_of_light = 2.99792458e+10/* cm / s */

	cgsm_gravitational_constant = 6.673e-8/* cm^3 / g s^2 */

	cgsm_plancks_constant_h = 6.62606896e-27/* g cm^2 / s */

	cgsm_plancks_constant_hbar = 1.05457162825e-27/* g cm^2 / s */

	cgsm_astronomical_unit = 1.49597870691e+13/* cm */

	cgsm_light_year = 9.46053620707e+17/* cm */

	cgsm_parsec = 3.08567758135e+18/* cm */

	cgsm_grav_accel = 9.80665e+2/* cm / s^2 */

	cgsm_electron_volt = 1.602176487e-12/* g cm^2 / s^2 */

	cgsm_mass_electron = 9.10938188e-28/* g */

	cgsm_mass_muon = 1.88353109e-25/* g */

	cgsm_mass_proton = 1.67262158e-24/* g */

	cgsm_mass_neutron = 1.67492716e-24/* g */

	cgsm_rydberg = 2.17987196968e-11/* g cm^2 / s^2 */

	cgsm_boltzmann = 1.3806504e-16/* g cm^2 / k s^2 */

	cgsm_molar_gas = 8.314472e+7/* g cm^2 / k mol s^2 */

	cgsm_standard_gas_volume = 2.2710981e+4/* cm^3 / mol */

	cgsm_minute = 6e+1/* s */

	cgsm_hour = 3.6e+3/* s */

	cgsm_day = 8.64e+4/* s */

	cgsm_week = 6.048e+5/* s */

	cgsm_inch = 2.54e+0/* cm */

	cgsm_foot = 3.048e+1/* cm */

	cgsm_yard = 9.144e+1/* cm */

	cgsm_mile = 1.609344e+5/* cm */

	cgsm_nautical_mile = 1.852e+5/* cm */

	cgsm_fathom = 1.8288e+2/* cm */

	cgsm_mil = 2.54e-3/* cm */

	cgsm_point = 3.52777777778e-2/* cm */

	cgsm_texpoint = 3.51459803515e-2/* cm */

	cgsm_micron = 1e-4/* cm */

	cgsm_angstrom = 1e-8/* cm */

	cgsm_hectare = 1e+8/* cm^2 */

	cgsm_acre = 4.04685642241e+7/* cm^2 */

	cgsm_barn = 1e-24/* cm^2 */

	cgsm_liter = 1e+3/* cm^3 */

	cgsm_us_gallon = 3.78541178402e+3/* cm^3 */

	cgsm_quart = 9.46352946004e+2/* cm^3 */

	cgsm_pint = 4.73176473002e+2/* cm^3 */

	cgsm_cup = 2.36588236501e+2/* cm^3 */

	cgsm_fluid_ounce = 2.95735295626e+1/* cm^3 */

	cgsm_tablespoon = 1.47867647813e+1/* cm^3 */

	cgsm_teaspoon = 4.92892159375e+0/* cm^3 */

	cgsm_canadian_gallon = 4.54609e+3/* cm^3 */

	cgsm_uk_gallon = 4.546092e+3/* cm^3 */

	cgsm_miles_per_hour = 4.4704e+1/* cm / s */

	cgsm_kilometers_per_hour = 2.77777777778e+1/* cm / s */

	cgsm_knot = 5.14444444444e+1/* cm / s */

	cgsm_pound_mass = 4.5359237e+2/* g */

	cgsm_ounce_mass = 2.8349523125e+1/* g */

	cgsm_ton = 9.0718474e+5/* g */

	cgsm_metric_ton = 1e+6/* g */

	cgsm_uk_ton = 1.0160469088e+6/* g */

	cgsm_troy_ounce = 3.1103475e+1/* g */

	cgsm_carat = 2e-1/* g */

	cgsm_unified_atomic_mass = 1.660538782e-24/* g */

	cgsm_gram_force = 9.80665e+2/* cm g / s^2 */

	cgsm_pound_force = 4.44822161526e+5/* cm g / s^2 */

	cgsm_kilopound_force = 4.44822161526e+8/* cm g / s^2 */

	cgsm_poundal = 1.38255e+4/* cm g / s^2 */

	cgsm_calorie = 4.1868e+7/* g cm^2 / s^2 */

	cgsm_btu = 1.05505585262e+10/* g cm^2 / s^2 */

	cgsm_therm = 1.05506e+15/* g cm^2 / s^2 */

	cgsm_horsepower = 7.457e+9/* g cm^2 / s^3 */

	cgsm_bar = 1e+6/* g / cm s^2 */

	cgsm_std_atmosphere = 1.01325e+6/* g / cm s^2 */

	cgsm_torr = 1.33322368421e+3/* g / cm s^2 */

	cgsm_meter_of_mercury = 1.33322368421e+6/* g / cm s^2 */

	cgsm_inch_of_mercury = 3.38638815789e+4/* g / cm s^2 */

	cgsm_inch_of_water = 2.490889e+3/* g / cm s^2 */

	cgsm_psi = 6.89475729317e+4/* g / cm s^2 */

	cgsm_poise = 1e+0/* g / cm s */

	cgsm_stokes = 1e+0/* cm^2 / s */

	cgsm_stilb = 1e+0/* cd / cm^2 */

	cgsm_lumen = 1e+0/* cd sr */

	cgsm_lux = 1e-4/* cd sr / cm^2 */

	cgsm_phot = 1e+0/* cd sr / cm^2 */

	cgsm_footcandle = 1.076e-3/* cd sr / cm^2 */

	cgsm_lambert = 1e+0/* cd sr / cm^2 */

	cgsm_footlambert = 1.07639104e-3/* cd sr / cm^2 */

	cgsm_curie = 3.7e+10/* 1 / s */

	cgsm_roentgen = 2.58e-8/* abamp s / g */

	cgsm_rad = 1e+2/* cm^2 / s^2 */

	cgsm_solar_mass = 1.98892e+33/* g */

	cgsm_bohr_radius = 5.291772083e-9/* cm */

	cgsm_newton = 1e+5/* cm g / s^2 */

	cgsm_dyne = 1e+0/* cm g / s^2 */

	cgsm_joule = 1e+7/* g cm^2 / s^2 */

	cgsm_erg = 1e+0/* g cm^2 / s^2 */

	cgsm_stefan_boltzmann_constant = 5.67040047374e-5/* g / k^4 s^3 */

	cgsm_thomson_cross_section = 6.65245893699e-25/* cm^2 */

	cgsm_bohr_magneton = 9.27400899e-21/* abamp cm^2 */

	cgsm_nuclear_magneton = 5.05078317e-24/* abamp cm^2 */

	cgsm_electron_magnetic_moment = 9.28476362e-21/* abamp cm^2 */

	cgsm_proton_magnetic_moment = 1.410606633e-23/* abamp cm^2 */

	cgsm_faraday = 9.64853429775e+3/* abamp s / mol */

	cgsm_electron_charge = 1.602176487e-20/* abamp s */

)
