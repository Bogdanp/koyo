import { Box, Card, HStack, Separator, Spacer, Text } from "@chakra-ui/react";
import React from "react";
import { useLocation, useNavigate } from "react-router";

import { Job } from "../api";
import { JobTable } from "./job-table";
import { BreadcrumbCurrentLink, BreadcrumbRoot } from "./ui/breadcrumb";
import { Button } from "./ui/button";

export interface JobsProps {
  jobs: Job[];
}

export const Jobs = (props: JobsProps) => {
  const { jobs } = props;
  const navigate = useNavigate();
  const location = useLocation();
  const searchParams = React.useMemo(
    () => new URLSearchParams(location.search),
    [location],
  );
  const onNextPage = React.useCallback(() => {
    const params = new URLSearchParams(searchParams);
    params.set("cursor", String(jobs[jobs.length - 1].id));
    navigate(`/?${params}`);
  }, [jobs, searchParams]);
  return (
    <Card.Root flex="1">
      <Card.Header p="3">
        <BreadcrumbRoot>
          <BreadcrumbCurrentLink>Jobs</BreadcrumbCurrentLink>
        </BreadcrumbRoot>
      </Card.Header>
      <Separator />
      {jobs.length === 0 ? (
        <Text color="fg.muted" p="4">
          There are no jobs matching these filters.
        </Text>
      ) : (
        <Box overflow="scroll">
          <JobTable jobs={jobs} />
          {jobs.length === 100 && (
            <HStack p="3">
              <Spacer />
              <Button onClick={onNextPage} variant="outline">
                Next Page
              </Button>
            </HStack>
          )}
        </Box>
      )}
    </Card.Root>
  );
};
