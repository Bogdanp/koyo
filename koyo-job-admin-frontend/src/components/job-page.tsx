import {
  Card,
  HStack,
  Separator,
  Spacer,
  Stack,
  Table,
  Text,
} from "@chakra-ui/react";
import * as React from "react";
import { LuCode, LuHash, LuRefreshCw, LuTrash } from "react-icons/lu";
import { LoaderFunctionArgs, useLoaderData, useNavigate } from "react-router";
import { Link as RouterLink } from "react-router";

import { Job, deleteJob, getJob, retryJob } from "../api";
import { FunctionCall } from "./function-call";
import { Layout } from "./layout";
import { RelativeDate } from "./relative-date";
import { StatusBadge } from "./status-badge";
import {
  BreadcrumbCurrentLink,
  BreadcrumbLink,
  BreadcrumbRoot,
} from "./ui/breadcrumb";
import { Button } from "./ui/button";

interface MetadataAttribute {
  label: string;
  cell: (job: Job) => React.ReactNode;
}

const ATTRIBUTES: MetadataAttribute[] = [
  { label: "Queue", cell: (j) => j.queue },
  { label: "Status", cell: (j) => <StatusBadge status={j.status} /> },
  { label: "Priority", cell: (j) => j.priority },
  { label: "Attempts", cell: (j) => j.attempts },
  {
    label: "Created At",
    cell: (j) => <RelativeDate addSuffix date={j["created-at"]} extended />,
  },
  {
    label: "Scheduled At",
    cell: (j) => <RelativeDate addSuffix date={j["scheduled-at"]} extended />,
  },
  {
    label: "Started At",
    cell: (j) => <RelativeDate addSuffix date={j["started-at"]} extended />,
  },
];

export const loadJobPage = async ({
  params,
}: LoaderFunctionArgs): Promise<Job> => {
  return await getJob(Number(params.id));
};

export const JobPage = () => {
  const job = useLoaderData();
  const navigate = useNavigate();
  const onRetry = React.useCallback(async () => {
    if (confirm("Retry this job?")) {
      await retryJob(job.id);
      navigate(0);
    }
  }, [job]);
  const onDelete = React.useCallback(async () => {
    if (confirm("Delete this job?")) {
      await deleteJob(job.id);
      navigate("/");
    }
  }, [job]);
  return (
    <Layout>
      <Card.Root m="4">
        <Card.Header p="3">
          <BreadcrumbRoot>
            <BreadcrumbLink asChild>
              <RouterLink to="/">Jobs</RouterLink>
            </BreadcrumbLink>
            <BreadcrumbCurrentLink>{job.name}</BreadcrumbCurrentLink>
          </BreadcrumbRoot>
        </Card.Header>
        <Separator />
        <Card.Body>
          <Stack direction={{ base: "column", md: "row" }} gap="8">
            <Stack w="100%" maxW="md">
              <HStack color="fg" fontSize="xl" fontWeight="semibold">
                <LuHash />
                <Text>Metadata</Text>
                <Spacer />
                <Button size="xs" variant="outline" onClick={onRetry}>
                  <LuRefreshCw />
                  Retry
                </Button>
                <Button
                  colorPalette="red"
                  size="xs"
                  variant="solid"
                  onClick={onDelete}
                >
                  <LuTrash />
                  Delete
                </Button>
              </HStack>
              <Table.Root>
                <Table.Body>
                  {ATTRIBUTES.map((attr) => (
                    <Table.Row key={attr.label}>
                      <Table.Cell
                        alignItems="top"
                        color="fg.muted"
                        fontSize="xs"
                        fontVariantNumeric="tabular-nums"
                        p="0"
                        textTransform="uppercase"
                      >
                        {attr.label}
                      </Table.Cell>
                      <Table.Cell>{attr.cell(job)}</Table.Cell>
                    </Table.Row>
                  ))}
                </Table.Body>
              </Table.Root>
            </Stack>
            <Stack w="100%" maxW="md">
              <HStack color="fg" fontSize="xl" fontWeight="semibold">
                <LuCode />
                <Text>Arguments</Text>
              </HStack>
              <FunctionCall fontSize="md" job={job} />
            </Stack>
          </Stack>
        </Card.Body>
      </Card.Root>
    </Layout>
  );
};
