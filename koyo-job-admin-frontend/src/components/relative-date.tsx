import { Text, type TextProps } from "@chakra-ui/react";
import { formatDistance } from "date-fns";
import * as React from "react";

export interface RelativeDateProps extends TextProps {
  addSuffix?: boolean;
  date: string;
}

export const RelativeDate = (props: RelativeDateProps) => {
  const { date, addSuffix, ...root } = props;
  return (
    <Text asChild {...root}>
      <span title={date}>
        {formatDistance(date, new Date(), { addSuffix })}
      </span>
    </Text>
  );
};
