source("r_clear_and_load.r")

conn <- sail_connect()

# Get cohort ===================================================================
cat("Get cohort\n")

q_cohort <- "
SELECT
	*
FROM sailw1151v.dcp02_disparity_cohort
;"

d_cohort <- sail_run(conn, q_cohort)

# Save =========================================================================
cat("Saving...\n")

qsave(
	d_cohort,
	file = s_drive("d_cohort.qs")
)
# Close connection
sail_close(conn)

cat("Done!\n")
beep(0)