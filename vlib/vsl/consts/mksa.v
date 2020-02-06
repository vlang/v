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
	mksa_speed_of_light = 2.99792458e+8/* m / s */

	mksa_gravitational_constant = 6.673e-11/* m^3 / kg s^2 */

	mksa_plancks_constant_h = 6.62606896e-34/* kg m^2 / s */

	mksa_plancks_constant_hbar = 1.05457162825e-34/* kg m^2 / s */

	mksa_astronomical_unit = 1.49597870691e+11/* m */

	mksa_light_year = 9.46053620707e+15/* m */

	mksa_parsec = 3.08567758135e+16/* m */

	mksa_grav_accel = 9.80665e+0/* m / s^2 */

	mksa_electron_volt = 1.602176487e-19/* kg m^2 / s^2 */

	mksa_mass_electron = 9.10938188e-31/* kg */

	mksa_mass_muon = 1.88353109e-28/* kg */

	mksa_mass_proton = 1.67262158e-27/* kg */

	mksa_mass_neutron = 1.67492716e-27/* kg */

	mksa_rydberg = 2.17987196968e-18/* kg m^2 / s^2 */

	mksa_boltzmann = 1.3806504e-23/* kg m^2 / k s^2 */

	mksa_molar_gas = 8.314472e+0/* kg m^2 / k mol s^2 */

	mksa_standard_gas_volume = 2.2710981e-2/* m^3 / mol */

	mksa_minute = 6e+1/* s */

	mksa_hour = 3.6e+3/* s */

	mksa_day = 8.64e+4/* s */

	mksa_week = 6.048e+5/* s */

	mksa_inch = 2.54e-2/* m */

	mksa_foot = 3.048e-1/* m */

	mksa_yard = 9.144e-1/* m */

	mksa_mile = 1.609344e+3/* m */

	mksa_nautical_mile = 1.852e+3/* m */

	mksa_fathom = 1.8288e+0/* m */

	mksa_mil = 2.54e-5/* m */

	mksa_point = 3.52777777778e-4/* m */

	mksa_texpoint = 3.51459803515e-4/* m */

	mksa_micron = 1e-6/* m */

	mksa_angstrom = 1e-10/* m */

	mksa_hectare = 1e+4/* m^2 */

	mksa_acre = 4.04685642241e+3/* m^2 */

	mksa_barn = 1e-28/* m^2 */

	mksa_liter = 1e-3/* m^3 */

	mksa_us_gallon = 3.78541178402e-3/* m^3 */

	mksa_quart = 9.46352946004e-4/* m^3 */

	mksa_pint = 4.73176473002e-4/* m^3 */

	mksa_cup = 2.36588236501e-4/* m^3 */

	mksa_fluid_ounce = 2.95735295626e-5/* m^3 */

	mksa_tablespoon = 1.47867647813e-5/* m^3 */

	mksa_teaspoon = 4.92892159375e-6/* m^3 */

	mksa_canadian_gallon = 4.54609e-3/* m^3 */

	mksa_uk_gallon = 4.546092e-3/* m^3 */

	mksa_miles_per_hour = 4.4704e-1/* m / s */

	mksa_kilometers_per_hour = 2.77777777778e-1/* m / s */

	mksa_knot = 5.14444444444e-1/* m / s */

	mksa_pound_mass = 4.5359237e-1/* kg */

	mksa_ounce_mass = 2.8349523125e-2/* kg */

	mksa_ton = 9.0718474e+2/* kg */

	mksa_metric_ton = 1e+3/* kg */

	mksa_uk_ton = 1.0160469088e+3/* kg */

	mksa_troy_ounce = 3.1103475e-2/* kg */

	mksa_carat = 2e-4/* kg */

	mksa_unified_atomic_mass = 1.660538782e-27/* kg */

	mksa_gram_force = 9.80665e-3/* kg m / s^2 */

	mksa_pound_force = 4.44822161526e+0/* kg m / s^2 */

	mksa_kilopound_force = 4.44822161526e+3/* kg m / s^2 */

	mksa_poundal = 1.38255e-1/* kg m / s^2 */

	mksa_calorie = 4.1868e+0/* kg m^2 / s^2 */

	mksa_btu = 1.05505585262e+3/* kg m^2 / s^2 */

	mksa_therm = 1.05506e+8/* kg m^2 / s^2 */

	mksa_horsepower = 7.457e+2/* kg m^2 / s^3 */

	mksa_bar = 1e+5/* kg / m s^2 */

	mksa_std_atmosphere = 1.01325e+5/* kg / m s^2 */

	mksa_torr = 1.33322368421e+2/* kg / m s^2 */

	mksa_meter_of_mercury = 1.33322368421e+5/* kg / m s^2 */

	mksa_inch_of_mercury = 3.38638815789e+3/* kg / m s^2 */

	mksa_inch_of_water = 2.490889e+2/* kg / m s^2 */

	mksa_psi = 6.89475729317e+3/* kg / m s^2 */

	mksa_poise = 1e-1/* kg m^-1 s^-1 */

	mksa_stokes = 1e-4/* m^2 / s */

	mksa_stilb = 1e+4/* cd / m^2 */

	mksa_lumen = 1e+0/* cd sr */

	mksa_lux = 1e+0/* cd sr / m^2 */

	mksa_phot = 1e+4/* cd sr / m^2 */

	mksa_footcandle = 1.076e+1/* cd sr / m^2 */

	mksa_lambert = 1e+4/* cd sr / m^2 */

	mksa_footlambert = 1.07639104e+1/* cd sr / m^2 */

	mksa_curie = 3.7e+10/* 1 / s */

	mksa_roentgen = 2.58e-4/* a s / kg */

	mksa_rad = 1e-2/* m^2 / s^2 */

	mksa_solar_mass = 1.98892e+30/* kg */

	mksa_bohr_radius = 5.291772083e-11/* m */

	mksa_newton = 1e+0/* kg m / s^2 */

	mksa_dyne = 1e-5/* kg m / s^2 */

	mksa_joule = 1e+0/* kg m^2 / s^2 */

	mksa_erg = 1e-7/* kg m^2 / s^2 */

	mksa_stefan_boltzmann_constant = 5.67040047374e-8/* kg / k^4 s^3 */

	mksa_thomson_cross_section = 6.65245893699e-29/* m^2 */

	mksa_bohr_magneton = 9.27400899e-24/* a m^2 */

	mksa_nuclear_magneton = 5.05078317e-27/* a m^2 */

	mksa_electron_magnetic_moment = 9.28476362e-24/* a m^2 */

	mksa_proton_magnetic_moment = 1.410606633e-26/* a m^2 */

	mksa_faraday = 9.64853429775e+4/* a s / mol */

	mksa_electron_charge = 1.602176487e-19/* a s */

	mksa_vacuum_permittivity = 8.854187817e-12/* a^2 s^4 / kg m^3 */

	mksa_vacuum_permeability = 1.25663706144e-6/* kg m / a^2 s^2 */

	mksa_debye = 3.33564095198e-30/* a s^2 / m^2 */

	mksa_gauss = 1e-4/* kg / a s^2 */

)
