/*
 * (C) Copyright IBM Corp. 2016,2020
 * 
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.utils.log;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.spi.LoggingEvent;

/**
 * Custom PatternLayout to make sure no sensitive information is logged.
 * 
 */
public class DeidPatternLayout extends PatternLayout {
  private static final String MASK = ": *******************";
  private static final Pattern PATTERN =
      Pattern.compile("(?i)(password|pass|api_key|apikey|ANNOTATOR_FOR_CLINICAL_DATA_KEY)");

  @Override
  public String format(LoggingEvent event) {
    if (event.getMessage() instanceof String) {
      String message = event.getRenderedMessage();
      Matcher matcher = PATTERN.matcher(message);

      if (matcher.find()) {
        int end = matcher.end();
        String maskedMessage = message.substring(0, end) + MASK;
        Throwable throwable =
            event.getThrowableInformation() != null ? event.getThrowableInformation().getThrowable()
                : null;
        LoggingEvent maskedEvent =
            new LoggingEvent(event.fqnOfCategoryClass, Logger.getLogger(event.getLoggerName()),
                event.timeStamp, event.getLevel(), maskedMessage, throwable);

        return super.format(maskedEvent);
      }
    }
    return super.format(event);
  }
}