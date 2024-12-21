import { Text, type TextProps } from "@chakra-ui/react";
import { formatDistance } from "date-fns";
import * as React from "react";

export interface RelativeDateProps extends TextProps {
  addSuffix?: boolean;
  extended?: boolean;
  date: string;
}

export const RelativeDate = (props: RelativeDateProps) => {
  const { addSuffix, date, extended, ...root } = props;
  return (
    <>
      <Text fontVariantNumeric="tabular-nums" title={date} {...root}>
        {formatDistance(date, new Date(), { addSuffix })}
      </Text>
      {extended && (
        <Text color="fg.muted" fontSize="xs" fontVariantNumeric="tabular-nums">
          {date}
        </Text>
      )}
    </>
  );
};
