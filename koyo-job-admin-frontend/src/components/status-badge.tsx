import { Badge } from "@chakra-ui/react";
import * as React from "react";

import { JobStatus } from "../api";

export interface StatusBadgeProps {
  status: JobStatus;
}

export const StatusBadge = (props: StatusBadgeProps) => {
  const { status } = props;
  const palette = React.useMemo(() => {
    switch (status) {
      case JobStatus.ready:
        return "blue.subtle";
      case JobStatus.running:
        return "blue";
      case JobStatus.done:
        return "green";
      case JobStatus.failed:
        return "red";
      default:
        return "fg";
    }
  }, [status]);
  return (
    <Badge colorPalette={palette} variant="surface">
      {status}
    </Badge>
  );
};
