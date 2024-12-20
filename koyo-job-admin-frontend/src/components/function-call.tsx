import { Stack, Text } from "@chakra-ui/react";
import * as React from "react";

import { Job } from "../api";

export interface FunctionCallProps {
  fontSize: "xs" | "sm" | "md" | "lg";
  job: Job;
}

export const FunctionCall = (props: FunctionCallProps) => {
  const { fontSize, job } = props;
  return (
    <Stack gap="0">
      <Text fontFamily="mono" fontSize={fontSize}>
        <Paren>{"("}</Paren>
        <strong>{job.name}</strong>
        {job.arguments.length === 0 && <Paren>{")"}</Paren>}
      </Text>
      {job.arguments.map((arg, idx) => (
        <Text
          asChild
          color="fg.muted"
          fontFamily="mono"
          fontSize={fontSize}
          key={idx}
          lineClamp={1}
        >
          <pre>
            {" "}
            {arg}
            {idx === job.arguments.length - 1 && <Paren>{")"}</Paren>}
          </pre>
        </Text>
      ))}
    </Stack>
  );
};

interface ParenProps {
  children?: React.ReactNode;
}

const Paren = (props: ParenProps) => {
  return (
    <Text asChild color="fg.muted">
      <span>{props.children}</span>
    </Text>
  );
};
