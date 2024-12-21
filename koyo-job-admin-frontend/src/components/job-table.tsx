import { Link, Table } from "@chakra-ui/react";
import * as React from "react";
import { Link as RouterLink } from "react-router";

import { Job } from "../api";
import { RelativeDate } from "./relative-date";
import { StatusBadge } from "./status-badge";

export interface JobTableProps {
  jobs: Job[];
}

export const JobTable = (props: JobTableProps) => {
  const { jobs } = props;
  return (
    <Table.Root interactive>
      <Table.Header>
        <Table.Row>
          <Table.ColumnHeader>Job</Table.ColumnHeader>
          <Table.ColumnHeader>Queue</Table.ColumnHeader>
          <Table.ColumnHeader>Status</Table.ColumnHeader>
          <Table.ColumnHeader textAlign="right">Priority</Table.ColumnHeader>
          <Table.ColumnHeader textAlign="right">Attempts</Table.ColumnHeader>
          <Table.ColumnHeader>Due</Table.ColumnHeader>
        </Table.Row>
      </Table.Header>
      <Table.Body>
        {jobs.map((job) => (
          <Table.Row key={job.id}>
            <Table.Cell>
              <Link asChild>
                <RouterLink to={`/jobs/${job.id}`}>{job.name}</RouterLink>
              </Link>
            </Table.Cell>
            <Table.Cell>{job.queue}</Table.Cell>
            <Table.Cell>
              <StatusBadge status={job.status} />
            </Table.Cell>
            <Table.Cell textAlign="right" fontVariantNumeric="tabular-nums">
              {job.priority}
            </Table.Cell>
            <Table.Cell textAlign="right" fontVariantNumeric="tabular-nums">
              {job.attempts}
            </Table.Cell>
            <Table.Cell>
              <RelativeDate
                addSuffix
                color="fg.muted"
                date={job["scheduled-at"]}
              />
            </Table.Cell>
          </Table.Row>
        ))}
      </Table.Body>
    </Table.Root>
  );
};
