// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module quic

import net
import time

// MigrationStats tracks migration statistics
pub struct MigrationStats {
pub mut:
	total_migrations      u64
	successful_migrations u64
	network_changes       u64
	nat_rebindings        u64
	path_degradations     u64
	manual_migrations     u64
	peer_migrations       u64
}

// success_rate calculates and returns the migration success rate as a fraction.
pub fn (stats &MigrationStats) success_rate() f64 {
	if stats.total_migrations == 0 {
		return 0.0
	}
	return f64(stats.successful_migrations) / f64(stats.total_migrations)
}

// MigrationPolicy defines when to trigger migration
pub struct MigrationPolicy {
pub:
	auto_migrate_on_network_change bool          = true
	auto_migrate_on_degradation    bool          = true
	packet_loss_threshold          f64           = 0.05 // 5%
	rtt_threshold                  time.Duration = 500 * time.millisecond
	probe_interval                 time.Duration = 30 * time.second
}

// MigrationController manages migration policy and decisions
pub struct MigrationController {
pub mut:
	migration ConnectionMigration
	policy    MigrationPolicy
	stats     MigrationStats
}

// new_migration_controller creates a new migration controller with the given addresses and policy.
pub fn new_migration_controller(local_addr net.Addr, remote_addr net.Addr, policy MigrationPolicy) MigrationController {
	return MigrationController{
		migration: new_connection_migration(local_addr, remote_addr)
		policy:    policy
		stats:     MigrationStats{}
	}
}

// evaluate evaluates whether migration should be triggered
pub fn (mut mc MigrationController) evaluate(packet_loss_rate f64, rtt time.Duration) !bool {
	if !mc.policy.auto_migrate_on_degradation {
		return false
	}

	if mc.migration.detect_path_degradation(packet_loss_rate, rtt) {
		// Try to find better path
		if best_path := mc.migration.select_best_path() {
			mc.migration.migrate_to_path(best_path)!
			mc.stats.successful_migrations++
			return true
		}
	}

	return false
}
