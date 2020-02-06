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
	cgs_speed_of_light = 2.99792458e+10/* cm / s */

	cgs_gravitational_constant = 6.673e-8/* cm^3 / g s^2 */

	cgs_plancks_constant_h = 6.62606896e-27/* g cm^2 / s */

	cgs_plancks_constant_hbar = 1.05457162825e-27/* g cm^2 / s */

	cgs_astronomical_unit = 1.49597870691e+13/* cm */

	cgs_light_year = 9.46053620707e+17/* cm */

	cgs_parsec = 3.08567758135e+18/* cm */

	cgs_grav_accel = 9.80665e+2/* cm / s^2 */

	cgs_electron_volt = 1.602176487e-12/* g cm^2 / s^2 */

	cgs_mass_electron = 9.10938188e-28/* g */

	cgs_mass_muon = 1.88353109e-25/* g */

	cgs_mass_proton = 1.67262158e-24/* g */

	cgs_mass_neutron = 1.67492716e-24/* g */

	cgs_rydberg = 2.17987196968e-11/* g cm^2 / s^2 */

	cgs_boltzmann = 1.3806504e-16/* g cm^2 / k s^2 */

	cgs_molar_gas = 8.314472e+7/* g cm^2 / k mol s^2 */

	cgs_standard_gas_volume = 2.2710981e+4/* cm^3 / mol */

	cgs_minute = 6e+1/* s */

	cgs_hour = 3.6e+3/* s */

	cgs_day = 8.64e+4/* s */

	cgs_week = 6.048e+5/* s */

	cgs_inch = 2.54e+0/* cm */

	cgs_foot = 3.048e+1/* cm */

	cgs_yard = 9.144e+1/* cm */

	cgs_mile = 1.609344e+5/* cm */

	cgs_nautical_mile = 1.852e+5/* cm */

	cgs_fathom = 1.8288e+2/* cm */

	cgs_mil = 2.54e-3/* cm */

	cgs_point = 3.52777777778e-2/* cm */

	cgs_texpoint = 3.51459803515e-2/* cm */

	cgs_micron = 1e-4/* cm */

	cgs_angstrom = 1e-8/* cm */

	cgs_hectare = 1e+8/* cm^2 */

	cgs_acre = 4.04685642241e+7/* cm^2 */

	cgs_barn = 1e-24/* cm^2 */

	cgs_liter = 1e+3/* cm^3 */

	cgs_us_gallon = 3.78541178402e+3/* cm^3 */

	cgs_quart = 9.46352946004e+2/* cm^3 */

	cgs_pint = 4.73176473002e+2/* cm^3 */

	cgs_cup = 2.36588236501e+2/* cm^3 */

	cgs_fluid_ounce = 2.95735295626e+1/* cm^3 */

	cgs_tablespoon = 1.47867647813e+1/* cm^3 */

	cgs_teaspoon = 4.92892159375e+0/* cm^3 */

	cgs_canadian_gallon = 4.54609e+3/* cm^3 */

	cgs_uk_gallon = 4.546092e+3/* cm^3 */

	cgs_miles_per_hour = 4.4704e+1/* cm / s */

	cgs_kilometers_per_hour = 2.77777777778e+1/* cm / s */

	cgs_knot = 5.14444444444e+1/* cm / s */

	cgs_pound_mass = 4.5359237e+2/* g */

	cgs_ounce_mass = 2.8349523125e+1/* g */

	cgs_ton = 9.0718474e+5/* g */

	cgs_metric_ton = 1e+6/* g */

	cgs_uk_ton = 1.0160469088e+6/* g */

	cgs_troy_ounce = 3.1103475e+1/* g */

	cgs_carat = 2e-1/* g */

	cgs_unified_atomic_mass = 1.660538782e-24/* g */

	cgs_gram_force = 9.80665e+2/* cm g / s^2 */

	cgs_pound_force = 4.44822161526e+5/* cm g / s^2 */

	cgs_kilopound_force = 4.44822161526e+8/* cm g / s^2 */

	cgs_poundal = 1.38255e+4/* cm g / s^2 */

	cgs_calorie = 4.1868e+7/* g cm^2 / s^2 */

	cgs_btu = 1.05505585262e+10/* g cm^2 / s^2 */

	cgs_therm = 1.05506e+15/* g cm^2 / s^2 */

	cgs_horsepower = 7.457e+9/* g cm^2 / s^3 */

	cgs_bar = 1e+6/* g / cm s^2 */

	cgs_std_atmosphere = 1.01325e+6/* g / cm s^2 */

	cgs_torr = 1.33322368421e+3/* g / cm s^2 */

	cgs_meter_of_mercury = 1.33322368421e+6/* g / cm s^2 */

	cgs_inch_of_mercury = 3.38638815789e+4/* g / cm s^2 */

	cgs_inch_of_water = 2.490889e+3/* g / cm s^2 */

	cgs_psi = 6.89475729317e+4/* g / cm s^2 */

	cgs_poise = 1e+0/* g / cm s */

	cgs_stokes = 1e+0/* cm^2 / s */

	cgs_stilb = 1e+0/* cd / cm^2 */

	cgs_lumen = 1e+0/* cd sr */

	cgs_lux = 1e-4/* cd sr / cm^2 */

	cgs_phot = 1e+0/* cd sr / cm^2 */

	cgs_footcandle = 1.076e-3/* cd sr / cm^2 */

	cgs_lambert = 1e+0/* cd sr / cm^2 */

	cgs_footlambert = 1.07639104e-3/* cd sr / cm^2 */

	cgs_curie = 3.7e+10/* 1 / s */

	cgs_roentgen = 2.58e-7/* a s / g */

	cgs_rad = 1e+2/* cm^2 / s^2 */

	cgs_solar_mass = 1.98892e+33/* g */

	cgs_bohr_radius = 5.291772083e-9/* cm */

	cgs_newton = 1e+5/* cm g / s^2 */

	cgs_dyne = 1e+0/* cm g / s^2 */

	cgs_joule = 1e+7/* g cm^2 / s^2 */

	cgs_erg = 1e+0/* g cm^2 / s^2 */

	cgs_stefan_boltzmann_constant = 5.67040047374e-5/* g / k^4 s^3 */

	cgs_thomson_cross_section = 6.65245893699e-25/* cm^2 */

)
