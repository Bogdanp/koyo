import { formatDistance } from "date-fns";
import * as React from "react";

export interface RelativeDateProps {
  date: string;
}

export const RelativeDate = (props: RelativeDateProps) => {
  return (
    <span title={props.date}>
      {formatDistance(props.date, new Date(), { addSuffix: true })}
    </span>
  );
};
