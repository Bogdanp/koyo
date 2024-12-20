import {
  Card,
  HStack,
  LinkBox,
  LinkOverlay,
  Separator,
  Spacer,
  Text,
} from "@chakra-ui/react";
import * as React from "react";
import { Link as RouterLink, useLocation } from "react-router";

import { Queue } from "../api";

export interface QueueListProps {
  queues: Queue[];
}

export const QueueList = (props: QueueListProps) => {
  const { queues } = props;
  const location = useLocation();
  const searchParams = React.useMemo(
    () => new URLSearchParams(location.search),
    [location],
  );
  const selectedQueue = React.useMemo(() => {
    return searchParams.get("queue");
  }, [searchParams]);
  const makeQueueLink = React.useCallback(
    (queue: string) => {
      const params = new URLSearchParams(searchParams);
      if (params.get("queue") === queue) {
        params.delete("queue");
      } else {
        params.set("queue", queue);
      }
      return `/?${params}`;
    },
    [searchParams],
  );
  return (
    <Card.Root>
      <Card.Header p="3">
        <HStack>
          <Text>Queues</Text>
          <Spacer />
          <Text color="fg.muted" fontSize="xs">
            BACKLOG
          </Text>
        </HStack>
      </Card.Header>
      <Separator />
      <Card.Body p="2">
        {queues.length === 0 ? (
          <Text color="fg.muted" m="1">
            No known queues.
          </Text>
        ) : (
          queues.map((q) => (
            <LinkBox key={q.id}>
              <HStack
                _hover={{ background: "bg.muted" }}
                background={selectedQueue === q.id ? "bg.muted" : "bg"}
                borderRadius="5px"
                p="3px 6px"
              >
                <LinkOverlay asChild>
                  <RouterLink to={makeQueueLink(q.id)}>{q.id}</RouterLink>
                </LinkOverlay>
                <Spacer />
                <Text
                  color="fg.muted"
                  fontSize="xs"
                  fontVariantNumeric="tabular-nums"
                >
                  {q["total-ready"]}
                </Text>
              </HStack>
            </LinkBox>
          ))
        )}
      </Card.Body>
    </Card.Root>
  );
};
