import {
  Card,
  HStack,
  LinkBox,
  LinkOverlay,
  Separator,
  Spacer,
  Stack,
  Text,
} from "@chakra-ui/react";
import * as React from "react";
import { Link as RouterLink, useLocation } from "react-router";

import { JobStatus, Queue } from "../api";
import { StatusBadge } from "./status-badge";

export interface JobStatusesProps {
  queues: Queue[];
}

export const JobStatuses = (props: JobStatusesProps) => {
  const { queues } = props;
  const location = useLocation();
  const searchParams = React.useMemo(
    () => new URLSearchParams(location.search),
    [location],
  );
  const selectedStatuses: JobStatus[] = React.useMemo(() => {
    return (searchParams.get("status") || "")
      .split(",")
      .filter((s) => s !== "") as JobStatus[];
  }, [searchParams]);
  const countsByStatus = React.useMemo(() => {
    const counts: Record<JobStatus, number> = {
      [JobStatus.ready]: 0,
      [JobStatus.running]: 0,
      [JobStatus.done]: 0,
      [JobStatus.failed]: 0,
    };
    for (const queue of queues) {
      counts[JobStatus.ready] += queue["total-ready"];
      counts[JobStatus.running] += queue["total-running"];
      counts[JobStatus.done] += queue["total-done"];
      counts[JobStatus.failed] += queue["total-failed"];
    }
    return counts;
  }, [queues]);
  const linkForStatus = React.useCallback(
    (status: JobStatus): string => {
      const params = new URLSearchParams(searchParams);
      let statuses = [...selectedStatuses];
      if (statuses.includes(status)) {
        statuses = statuses.filter((s) => s !== status);
      } else {
        statuses.push(status);
      }
      if (statuses.length === 0) {
        params.delete("status");
      } else {
        params.set("status", statuses.join(","));
      }
      return `/?${params}`;
    },
    [searchParams, selectedStatuses],
  );
  return (
    <Card.Root>
      <Card.Header p="3">
        <HStack>
          <Text>Statuses</Text>
          <Spacer />
          <Text color="fg.muted" fontSize="xs">
            COUNT
          </Text>
        </HStack>
      </Card.Header>
      <Separator />
      <Card.Body p="2">
        <Stack gap="1">
          {Object.values(JobStatus).map((status) => (
            <LinkBox key={status}>
              <HStack
                _hover={{ background: "bg.muted" }}
                background={
                  selectedStatuses.includes(status) ? "bg.muted" : "bg"
                }
                borderRadius="5px"
                p="3px 5px"
              >
                <LinkOverlay asChild>
                  <RouterLink to={linkForStatus(status)}>
                    <StatusBadge status={status} />
                  </RouterLink>
                </LinkOverlay>
                <Spacer />
                <Text
                  color="fg.muted"
                  fontSize="xs"
                  fontVariantNumeric="tabular-nums"
                >
                  {countsByStatus[status]}
                </Text>
              </HStack>
            </LinkBox>
          ))}
        </Stack>
      </Card.Body>
    </Card.Root>
  );
};
