import axios from "axios";

let root = "/_koyo/jobs";

export const setRoot = (r: string) => {
  root = r;
};

export const getRoot = (): string => {
  return root;
};

export enum JobStatus {
  ready = "ready",
  running = "running",
  done = "done",
  failed = "failed",
}

export interface Job {
  id: number;
  queue: string;
  name: string;
  arguments: string[];
  status: JobStatus;
  priority: number;
  attempts: number;
  "created-at": string;
  "scheduled-at": string;
  "started-at": string;
  "worker-id": string;
}

export const getJobs = async (
  params?: Record<string, string>,
): Promise<Job[]> => {
  return (await axios.get(`${root}/api/v1/jobs`, { params })).data;
};

export const getJob = async (id: number): Promise<Job> => {
  return (await axios.get(`${root}/api/v1/jobs/${id}`)).data;
};

export const retryJob = async (id: number): Promise<void> => {
  await axios.post(`${root}/api/v1/jobs/${id}/retry`);
};

export const deleteJob = async (id: number): Promise<void> => {
  await axios.delete(`${root}/api/v1/jobs/${id}`);
};

export interface Queue {
  id: string;
  "total-ready": number;
  "total-running": number;
  "total-done": number;
  "total-failed": number;
}

export const getQueues = async (): Promise<Queue[]> => {
  return (await axios.get(`${root}/api/v1/queues`)).data;
};

export interface Worker {
  id: number;
  pid: number;
  hostname: string;
  heartbeat: string;
  "up-since": string;
}

export const getWorkers = async (): Promise<Worker[]> => {
  return (await axios.get(`${root}/api/v1/workers`)).data;
};
