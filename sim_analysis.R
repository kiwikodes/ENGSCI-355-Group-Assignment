require(dplyr)
library(data.table)


# Read and preprocess data
naStrings = c('this.SimTime/1[h]', 'this.obj',
                '[Simulation].ReplicationNumber', 'this.obj.TotalTime / 1[h]')

data = read.table('complete_model-patient-event-logger.log',
                  sep="\t",
                  col.names=c('SimTime', 'Scenario', 'Replication',
                              'Object', 'Event', 'EventTime'),
                  skip=15,
                  na.strings=naStrings,
                  skipNul=TRUE)

data = na.omit(data)
data$EventTime = as.numeric(data$EventTime)
data$SimTime = as.numeric(data$SimTime)


# P1: 95% ED arrival -> discharge or ward stay < 6 hours
P1 = data |>
  group_by(Scenario, Replication, Object) |>
  summarise(TimeDiff = min(EventTime[Event %in% c('Wards.ward-stay', 'patient-leave')]) - 
            min(EventTime[Event %in% c('ED.wait-to-register', 'ED.wait-for-triage', 'ED.wait-for-consultation')]), 
            .groups = 'drop') |>
  select(Scenario, Replication, TimeDiff)


# P1: Raw quantiles (quantile for whole scenario i.e. irrespective of replication)
P1 |>
  group_by(Scenario) |>
  summarise(Quantile = quantile(TimeDiff, 0.95))

# P1: 95% CI for quantiles
P1 |>
  group_by(Scenario, Replication) |>
  summarise(Quantile = quantile(TimeDiff, 0.95), .groups = "drop") |>
  group_by(Scenario) |>
  summarise(lower95CI = t.test(Quantile)$conf.int[1], mean = mean(Quantile),
            upper95CI = t.test(Quantile)$conf.int[2])


# P2: Average ED need observing -> being observed < 2 mins
P2 = data |>
  group_by(Scenario, Replication, Object) |>
  mutate(EventEnd = lead(EventTime)) |>
  filter(Event %in% c('ED.observation', 'PatientTransit.dropoff', 'ED.triage') | row_number() == 1) |>
  mutate(ObsWaitTime = if_else(lag(Event) == 'ED.wait-for-consultation', 
                               (EventTime - lag(EventTime)) * 60, 
                               (EventTime - lag(EventEnd + 0.5)) * 60)) |>
  filter(Event == 'ED.observation') |>
  ungroup() |>
  select(Scenario, Replication, ObsWaitTime)

# P2: CI for averages
P2 |>
  group_by(Scenario) |>
  summarise(lower95CI = t.test(ObsWaitTime)$conf.int[1], mean = mean(ObsWaitTime),
            upper95CI = t.test(ObsWaitTime)$conf.int[2])


# P3: Average need pickup -> pickup less < 20 mins
P3 = data |>
  group_by(Scenario, Replication) |>
  filter(Event %in% c('PatientTransit.wait-for-assignment', 'PatientTransit.pickup')) |>
  mutate(WaitTime = (EventTime - lag(EventTime)) * 60) |>
  filter(Event == 'PatientTransit.pickup') |>
  select(Scenario, Replication, WaitTime)

# P3: CI for averages
P3 |>
  group_by(Scenario) |>
  summarise(lower95CI = t.test(WaitTime)$conf.int[1], mean = mean(WaitTime),
            upper95CI = t.test(WaitTime)$conf.int[2])


# P4: Average ward need observing -> being observed < 15 mins
P4 = data |>
  group_by(Scenario, Replication) |>
  mutate(EventEnd = lead(EventTime)) |>
  filter(Event %in% c('Wards.admission', 'Wards.observation')) |>
  mutate(ObsWaitTime = (EventTime - lag(EventEnd + 2)) * 60) |>
  filter(Event == 'Wards.observation') |>
  select(Scenario, Replication, ObsWaitTime)

# P4: Raw quantiles (quantile for whole scenario i.e. irrespective of replication)
P4 |>
  group_by(Scenario) |>
  summarise(Quantile = quantile(ObsWaitTime, 0.95))

# P4: CI for quantiles
P4 |>
  group_by(Scenario, Replication) |>
  summarise(Quantile = quantile(ObsWaitTime, 0.95), .groups = "drop") |>
  group_by(Scenario) |>
  summarise(lower95CI = t.test(Quantile)$conf.int[1], mean = mean(Quantile),
            upper95CI = t.test(Quantile)$conf.int[2])


# P5: Waiting for test < 5 mins
P5 = data |>
  group_by(Scenario, Replication) |>
  filter(Event %in% c('Wards.wait-for-test', 'Wards.perform-test')) |>
  mutate(WaitTime = (EventTime - lag(EventTime)) * 60) |>
  filter(Event == 'Wards.perform-test') |>
  select(Scenario, Replication, WaitTime)

# P5: CI for averages
P5 |>
  group_by(Scenario) |>
  summarise(lower95CI = t.test(WaitTime)$conf.int[1], mean = mean(WaitTime),
            upper95CI = t.test(WaitTime)$conf.int[2])
