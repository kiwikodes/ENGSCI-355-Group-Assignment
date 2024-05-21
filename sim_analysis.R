require(dplyr)
library(data.table)


nullStrings = c("this.SimTime/1[h]",  "this.obj",
                "[Simulation].ReplicationNumber", "this.obj.TotalTime / 1[h]")

data = read.table("complete_model-patientlogger_and_orderly/complete_model-patient-event-logger.log", 
                  sep="\t", 
                  col.names=c('SimTime_h', 'Scenario', 'Replication', 'Object', 'Event', 'EventTime'), 
                  skip=19, 
                  na.strings=nullStrings, 
                  skipNul=TRUE)

data = na.omit(data)
data$EventTime = as.numeric(data$EventTime)
data$SimTime_h = as.numeric(data$SimTime_h)


# The time between arrival to the ED and either being discharged from the ED or finishing being admitted
# to a ward, should be less than 6 hours for 95% of patients
time_diffs = data %>%
  filter(Event %in% c("ED.wait-to-register", "patient-leave")) %>%
  group_by(Replication, Object) %>%
  summarise(time_diff = diff(range(EventTime[Event %in% c("ED.wait-to-register", "patient-leave")])),
            .groups = 'drop')

time_diffs %>%
  summarise(lower95CI = t.test(time_diff)$conf.int[1],
            mean = mean(time_diff, na.rm = TRUE),
            upper95CI = t.test(time_diff)$conf.int[2])

# The time between needing to be observed and starting an observation in the ED, should be less than 2
# minutes on average.
pass

# The time between requesting a transit (starting to wait for an orderly to be assigned) and starting being
# picked up, should be less than 20 minutes on average
time_diffs = data %>%
  filter(Event %in% c("Wards.wait-for-test", "Wards.perform-test")) %>%
  group_by(Replication, Object) %>%
  summarise(time_diff = diff(range(EventTime[Event %in% c("Wards.wait-for-test", "Wards.perform-test")])),
            .groups = 'drop')

time_diffs %>%
  summarise(lower95CI = t.test(time_diff)$conf.int[1],
            mean = mean(time_diff, na.rm = TRUE),
            upper95CI = t.test(time_diff)$conf.int[2])

# The time between needing to be observed and starting an observation in the Wards, should be less than
# 15 minutes 95% of the time.

# The time spent waiting for a test should be less than 5 minutes on average
time_diffs = data %>%
  filter(Event %in% c("Wards.wait-for-test", "Wards.perform-test")) %>%
  group_by(Replication, Object) %>%
  summarise(time_diff = diff(range(EventTime[Event %in% c("Wards.wait-for-test", "Wards.perform-test")])),
            .groups = 'drop')

time_diffs %>%
  summarise(lower95CI = t.test(time_diff)$conf.int[1],
            mean = mean(time_diff, na.rm = TRUE),
            upper95CI = t.test(time_diff)$conf.int[2])
