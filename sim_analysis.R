require(dplyr)
library(data.table)


nullStrings = c("this.SimTime/1[h]",  "this.obj",
                "[Simulation].ReplicationNumber", "this.obj.TotalTime / 1[h]")

data = read.table("complete_model-patientlogger_and_orderly/complete_model-patient-event-logger.log", 
                  sep="\t", 
                  col.names=c('SimTime_h', 'Scenario', 'Replication', 'Object', 'Event', 'EventTime'), 
                  skip=16, 
                  na.strings=nullStrings, 
                  skipNul=TRUE)

data = na.omit(data)
data$EventTime = as.numeric(data$EventTime)
data$SimTime_h = as.numeric(data$SimTime_h)


# The time between arrival to the ED and either being discharged from the ED or finishing being admitted
# to a ward, should be less than 6 hours for 95% of patients

# Only works with the filter on line 31, and the na.rm on line 34 (need to look at)
P1 = data %>%
  group_by(Replication, Object) %>%
  summarise(
    first_event_time = first(EventTime),
    end_event_time = first(EventTime[Event %in% c("Wards.admission", "patient-leave")]),
    time_diff = end_event_time - first_event_time,
    .groups = 'drop'
  ) %>% filter(!is.na(time_diff))

P1$time_diff = sort(P1$time_diff)
P1$time_diff %>% quantile(P1$time_diff, probs=0.95, na.rm=TRUE)


# The time between needing to be observed and starting an observation in the ED, should be less than 2
# minutes on average.

## NEED TO COMPLETE

# The time between requesting a transit (starting to wait for an orderly to be assigned) and starting being
# picked up, should be less than 20 minutes on average
P3 = data %>%
  filter(Event %in% c("PatientTransit.wait-for-assignment", "PatientTransit.pickup")) %>%
  arrange(Object, EventTime) %>%
  group_by(Object) %>%
  mutate(event_id = cumsum(Event == "PatientTransit.wait-for-assignment")) %>%
  group_by(Object, event_id) %>%
  reframe(
    time_diff = diff(EventTime[Event %in% c("PatientTransit.wait-for-assignment", "PatientTransit.pickup")])
  )

mean(P3$time_diff)


# The time between needing to be observed and starting an observation in the Wards, should be less than
# 15 minutes 95% of the time.

## NEED TO COMPLETE

# The time spent waiting for a test should be less than 5 minutes on average
P5 = data %>%
  filter(Event %in% c("Wards.wait-for-test", "Wards.perform-test")) %>%
  group_by(Replication, Object) %>%
  summarise(time_diff = diff(range(EventTime[Event %in% c("Wards.wait-for-test", "Wards.perform-test")])),
            .groups = 'drop')

mean(P5$time_diff)
