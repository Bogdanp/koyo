import { Stack } from "@chakra-ui/react";
import * as React from "react";
import { LoaderFunctionArgs, useLoaderData } from "react-router";

import { Job, Queue, Worker, getJobs, getQueues, getWorkers } from "../api";
import { Jobs } from "./jobs";
import { Layout } from "./layout";
import { QueueList } from "./queue-list";
import { JobStatuses } from "./status-list";
import { WorkerList } from "./worker-list";

interface Data {
  jobs: Job[];
  queues: Queue[];
  workers: Worker[];
}

export const loadDashboardPage = async ({
  request,
}: LoaderFunctionArgs): Promise<Data> => {
  const url = new URL(request.url);
  const jobsPromise = getJobs({
    cursor: url.searchParams.get("cursor"),
    status: url.searchParams.get("status"),
    queue: url.searchParams.get("queue"),
  });
  const queuesPromise = getQueues();
  const workersPromise = getWorkers();
  const [jobs, queues, workers] = await Promise.all([
    jobsPromise,
    queuesPromise,
    workersPromise,
  ]);
  return { jobs, queues, workers };
};

export const DashboardPage = () => {
  const { jobs, queues, workers }: Data = useLoaderData();
  return (
    <Layout>
      <Stack flex="1" p="4" gap="4" direction={{ base: "column", md: "row" }}>
        <Stack gap="4" w="100%" maxW={{ base: "100%", md: "xs" }}>
          <JobStatuses queues={queues} />
          <QueueList queues={queues} />
          <WorkerList workers={workers} />
        </Stack>
        <Jobs jobs={jobs} />
      </Stack>
    </Layout>
  );
};
