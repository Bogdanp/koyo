import { Card, HStack, Separator, Spacer, Text } from "@chakra-ui/react";
import * as React from "react";

import { Worker } from "../api";
import { RelativeDate } from "./relative-date";

export interface WorkerListProps {
  workers: Worker[];
}

export const WorkerList = (props: WorkerListProps) => {
  const { workers } = props;
  return (
    <Card.Root>
      <Card.Header p="3">
        <HStack>
          <Text>Workers</Text>
          <Spacer />
          <Text color="fg.muted" fontSize="xs">
            LAST SEEN
          </Text>
        </HStack>
      </Card.Header>
      <Separator />
      <Card.Body p="3">
        {workers.length === 0 ? (
          <Text color="fg.muted">No workers.</Text>
        ) : (
          workers.map((w) => (
            <HStack key={w.id}>
              <WorkerName worker={w} />
              <Spacer />
              <RelativeDate color="fg.muted" date={w.heartbeat} fontSize="xs" />
            </HStack>
          ))
        )}
      </Card.Body>
    </Card.Root>
  );
};

interface WorkerNameProps {
  worker: Worker;
}

const WorkerName = (props: WorkerNameProps) => {
  const { worker } = props;
  return (
    <>
      <Text fontFamily="mono" fontVariantNumeric="tabular-nums" fontSize="xs">
        {worker.pid} @ {worker.hostname}
      </Text>
    </>
  );
};
