/*
 * (C) Copyright IBM Corp. 2016,2020
 * 
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.utils.log;

import static org.hamcrest.CoreMatchers.anyOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import org.apache.log4j.Appender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class DeidPatternLayoutTest {

  @Mock
  Appender mockAppender;

  @Captor
  private ArgumentCaptor<LoggingEvent> captorLoggingEvent;

  private static LogManager log = LogManager.getInstance();

  @Before
  public void setup() {
    Logger logger = Logger.getRootLogger();
    when(mockAppender.getName()).thenReturn("MOCK");
    logger.addAppender(mockAppender);
  }

  @Test	
  public void logPassword() {	
    DeidPatternLayout layout = new DeidPatternLayout();

    log.logInfo(LogCodes.WPH1013E, "The password is : super secret password");
    verify(mockAppender).doAppend(captorLoggingEvent.capture());
    LoggingEvent loggingEvent = captorLoggingEvent.getValue();
    assertThat(loggingEvent.getLevel(), is(Level.INFO));
    assertThat(layout.format(loggingEvent),
        anyOf(is(
            "WPH1013E - com.ibm.whc.deid.utils.log.DeidPatternLayoutTest - logPassword: *******************\r\n"),
            is("WPH1013E - com.ibm.whc.deid.utils.log.DeidPatternLayoutTest - logPassword: *******************\n")));

    log.logInfo(LogCodes.WPH1013E, "The key is : super secret key");
    loggingEvent = captorLoggingEvent.getValue();
    assertThat(loggingEvent.getLevel(), is(Level.INFO));
    assertThat(layout.format(loggingEvent),
        anyOf(is(
            "WPH1013E - com.ibm.whc.deid.utils.log.DeidPatternLayoutTest - logPassword: *******************\r\n"),
            is("WPH1013E - com.ibm.whc.deid.utils.log.DeidPatternLayoutTest - logPassword: *******************\n")));

    log.logInfo(LogCodes.WPH1013E, "The key and password is : super secret key");
    loggingEvent = captorLoggingEvent.getValue();
    assertThat(loggingEvent.getLevel(), is(Level.INFO));
    assertThat(layout.format(loggingEvent),
        anyOf(is(
            "WPH1013E - com.ibm.whc.deid.utils.log.DeidPatternLayoutTest - logPassword: *******************\r\n"),
            is("WPH1013E - com.ibm.whc.deid.utils.log.DeidPatternLayoutTest - logPassword: *******************\n")));

    log.logInfo(LogCodes.WPH1013E, "The password is : super secret key");
    loggingEvent = captorLoggingEvent.getValue();
    assertThat(loggingEvent.getLevel(), is(Level.INFO));
    assertThat(layout.format(loggingEvent),
        anyOf(is(
            "WPH1013E - com.ibm.whc.deid.utils.log.DeidPatternLayoutTest - logPassword: *******************\r\n"),
            is("WPH1013E - com.ibm.whc.deid.utils.log.DeidPatternLayoutTest - logPassword: *******************\n")));
  }
}
