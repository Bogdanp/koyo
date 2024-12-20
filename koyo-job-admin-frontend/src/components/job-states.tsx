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

export interface JobStatesProps {
  queues: Queue[];
}

export const JobStates = (props: JobStatesProps) => {
  const { queues } = props;
  const location = useLocation();
  const searchParams = React.useMemo(
    () => new URLSearchParams(location.search),
    [location],
  );
  const selectedStates: JobStatus[] = React.useMemo(() => {
    return (searchParams.get("state") || "")
      .split(",")
      .filter((s) => s !== "") as JobStatus[];
  }, [searchParams]);
  const countsByState = React.useMemo(() => {
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
  const linkForState = React.useCallback(
    (state: JobStatus): string => {
      const params = new URLSearchParams(searchParams);
      let states = [...selectedStates];
      if (states.includes(state)) {
        states = states.filter((s) => s !== state);
      } else {
        states.push(state);
      }
      if (states.length === 0) {
        params.delete("state");
      } else {
        params.set("state", states.join(","));
      }
      return `/?${params}`;
    },
    [searchParams, selectedStates],
  );
  return (
    <Card.Root>
      <Card.Header p="3">
        <HStack>
          <Text>States</Text>
          <Spacer />
          <Text color="fg.muted" fontSize="xs">
            COUNT
          </Text>
        </HStack>
      </Card.Header>
      <Separator />
      <Card.Body p="2">
        <Stack gap="1">
          {Object.values(JobStatus).map((state) => (
            <LinkBox key={state}>
              <HStack
                _hover={{ background: "bg.muted" }}
                background={selectedStates.includes(state) ? "bg.muted" : "bg"}
                borderRadius="5px"
                p="3px 5px"
              >
                <LinkOverlay asChild>
                  <RouterLink to={linkForState(state)}>
                    {titleCase(state)}
                  </RouterLink>
                </LinkOverlay>
                <Spacer />
                <Text
                  color="fg.muted"
                  fontSize="xs"
                  fontVariantNumeric="tabular-nums"
                >
                  {countsByState[state]}
                </Text>
              </HStack>
            </LinkBox>
          ))}
        </Stack>
      </Card.Body>
    </Card.Root>
  );
};

const titleCase = (s: string) => {
  if (s === "") return s;
  return s[0].toUpperCase() + s.substring(1);
};
