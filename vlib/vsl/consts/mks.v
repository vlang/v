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
	mks_speed_of_light = 2.99792458e+8/* m / s */

	mks_gravitational_constant = 6.673e-11/* m^3 / kg s^2 */

	mks_plancks_constant_h = 6.62606896e-34/* kg m^2 / s */

	mks_plancks_constant_hbar = 1.05457162825e-34/* kg m^2 / s */

	mks_astronomical_unit = 1.49597870691e+11/* m */

	mks_light_year = 9.46053620707e+15/* m */

	mks_parsec = 3.08567758135e+16/* m */

	mks_grav_accel = 9.80665e+0/* m / s^2 */

	mks_electron_volt = 1.602176487e-19/* kg m^2 / s^2 */

	mks_mass_electron = 9.10938188e-31/* kg */

	mks_mass_muon = 1.88353109e-28/* kg */

	mks_mass_proton = 1.67262158e-27/* kg */

	mks_mass_neutron = 1.67492716e-27/* kg */

	mks_rydberg = 2.17987196968e-18/* kg m^2 / s^2 */

	mks_boltzmann = 1.3806504e-23/* kg m^2 / k s^2 */

	mks_molar_gas = 8.314472e+0/* kg m^2 / k mol s^2 */

	mks_standard_gas_volume = 2.2710981e-2/* m^3 / mol */

	mks_minute = 6e+1/* s */

	mks_hour = 3.6e+3/* s */

	mks_day = 8.64e+4/* s */

	mks_week = 6.048e+5/* s */

	mks_inch = 2.54e-2/* m */

	mks_foot = 3.048e-1/* m */

	mks_yard = 9.144e-1/* m */

	mks_mile = 1.609344e+3/* m */

	mks_nautical_mile = 1.852e+3/* m */

	mks_fathom = 1.8288e+0/* m */

	mks_mil = 2.54e-5/* m */

	mks_point = 3.52777777778e-4/* m */

	mks_texpoint = 3.51459803515e-4/* m */

	mks_micron = 1e-6/* m */

	mks_angstrom = 1e-10/* m */

	mks_hectare = 1e+4/* m^2 */

	mks_acre = 4.04685642241e+3/* m^2 */

	mks_barn = 1e-28/* m^2 */

	mks_liter = 1e-3/* m^3 */

	mks_us_gallon = 3.78541178402e-3/* m^3 */

	mks_quart = 9.46352946004e-4/* m^3 */

	mks_pint = 4.73176473002e-4/* m^3 */

	mks_cup = 2.36588236501e-4/* m^3 */

	mks_fluid_ounce = 2.95735295626e-5/* m^3 */

	mks_tablespoon = 1.47867647813e-5/* m^3 */

	mks_teaspoon = 4.92892159375e-6/* m^3 */

	mks_canadian_gallon = 4.54609e-3/* m^3 */

	mks_uk_gallon = 4.546092e-3/* m^3 */

	mks_miles_per_hour = 4.4704e-1/* m / s */

	mks_kilometers_per_hour = 2.77777777778e-1/* m / s */

	mks_knot = 5.14444444444e-1/* m / s */

	mks_pound_mass = 4.5359237e-1/* kg */

	mks_ounce_mass = 2.8349523125e-2/* kg */

	mks_ton = 9.0718474e+2/* kg */

	mks_metric_ton = 1e+3/* kg */

	mks_uk_ton = 1.0160469088e+3/* kg */

	mks_troy_ounce = 3.1103475e-2/* kg */

	mks_carat = 2e-4/* kg */

	mks_unified_atomic_mass = 1.660538782e-27/* kg */

	mks_gram_force = 9.80665e-3/* kg m / s^2 */

	mks_pound_force = 4.44822161526e+0/* kg m / s^2 */

	mks_kilopound_force = 4.44822161526e+3/* kg m / s^2 */

	mks_poundal = 1.38255e-1/* kg m / s^2 */

	mks_calorie = 4.1868e+0/* kg m^2 / s^2 */

	mks_btu = 1.05505585262e+3/* kg m^2 / s^2 */

	mks_therm = 1.05506e+8/* kg m^2 / s^2 */

	mks_horsepower = 7.457e+2/* kg m^2 / s^3 */

	mks_bar = 1e+5/* kg / m s^2 */

	mks_std_atmosphere = 1.01325e+5/* kg / m s^2 */

	mks_torr = 1.33322368421e+2/* kg / m s^2 */

	mks_meter_of_mercury = 1.33322368421e+5/* kg / m s^2 */

	mks_inch_of_mercury = 3.38638815789e+3/* kg / m s^2 */

	mks_inch_of_water = 2.490889e+2/* kg / m s^2 */

	mks_psi = 6.89475729317e+3/* kg / m s^2 */

	mks_poise = 1e-1/* kg m^-1 s^-1 */

	mks_stokes = 1e-4/* m^2 / s */

	mks_stilb = 1e+4/* cd / m^2 */

	mks_lumen = 1e+0/* cd sr */

	mks_lux = 1e+0/* cd sr / m^2 */

	mks_phot = 1e+4/* cd sr / m^2 */

	mks_footcandle = 1.076e+1/* cd sr / m^2 */

	mks_lambert = 1e+4/* cd sr / m^2 */

	mks_footlambert = 1.07639104e+1/* cd sr / m^2 */

	mks_curie = 3.7e+10/* 1 / s */

	mks_roentgen = 2.58e-4/* a s / kg */

	mks_rad = 1e-2/* m^2 / s^2 */

	mks_solar_mass = 1.98892e+30/* kg */

	mks_bohr_radius = 5.291772083e-11/* m */

	mks_newton = 1e+0/* kg m / s^2 */

	mks_dyne = 1e-5/* kg m / s^2 */

	mks_joule = 1e+0/* kg m^2 / s^2 */

	mks_erg = 1e-7/* kg m^2 / s^2 */

	mks_stefan_boltzmann_constant = 5.67040047374e-8/* kg / k^4 s^3 */

	mks_thomson_cross_section = 6.65245893699e-29/* m^2 */

	mks_bohr_magneton = 9.27400899e-24/* a m^2 */

	mks_nuclear_magneton = 5.05078317e-27/* a m^2 */

	mks_electron_magnetic_moment = 9.28476362e-24/* a m^2 */

	mks_proton_magnetic_moment = 1.410606633e-26/* a m^2 */

	mks_faraday = 9.64853429775e+4/* a s / mol */

	mks_electron_charge = 1.602176487e-19/* a s */

	mks_vacuum_permittivity = 8.854187817e-12/* a^2 s^4 / kg m^3 */

	mks_vacuum_permeability = 1.25663706144e-6/* kg m / a^2 s^2 */

	mks_debye = 3.33564095198e-30/* a s^2 / m^2 */

	mks_gauss = 1e-4/* kg / a s^2 */

)
