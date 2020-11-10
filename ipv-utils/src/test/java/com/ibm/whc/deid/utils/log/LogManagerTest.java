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
public class LogManagerTest {

  @Mock
  Appender mockAppender;

  @Captor
  private ArgumentCaptor<LoggingEvent> captorLoggingEvent;

  private static LogManager log = LogManager.getInstance();

  @Before
  public void setup() {
    Logger logger = Logger.getRootLogger();
    when(mockAppender.getName()).thenReturn("MOCK");
    // when(mockAppender.isStarted()).thenReturn(true);
    logger.addAppender(mockAppender);
  }

  @Test
  public void testLogInfo() {
    log.logInfo(LogCodes.WPH1000I, "This is test message");

    verify(mockAppender).doAppend(captorLoggingEvent.capture());
    final LoggingEvent loggingEvent = captorLoggingEvent.getValue();
    assertThat(loggingEvent.getLevel(), is(Level.INFO));
    assertThat(loggingEvent.getMessage(), anyOf(is(
        "WPH1000I - com.ibm.whc.deid.utils.log.LogManager - logInfo - IPV-core information : \"This is test message\""),
        is("WPH1000I - com.ibm.whc.deid.utils.log.LogManagerTest - testLogInfo - IPV-core information : \"This is test message\"")));
  }

  @Test
  public void testLogWarn() {
    log.logWarn(LogCodes.WPH1012W, "This is test message");

    verify(mockAppender).doAppend(captorLoggingEvent.capture());
    final LoggingEvent loggingEvent = captorLoggingEvent.getValue();
    assertThat(loggingEvent.getLevel(), is(Level.WARN));
    assertThat(loggingEvent.getMessage(), anyOf(is(
        "WPH1012W - com.ibm.whc.deid.utils.log.LogManager - logWarn - IPV-core warning : \"This is test message\""),
        is("WPH1012W - com.ibm.whc.deid.utils.log.LogManagerTest - testLogWarn - IPV-core warning : \"This is test message\"")));
  }

  @Test
  public void testLogError() {
    log.logError(LogCodes.WPH1013E, "This is test message");

    verify(mockAppender).doAppend(captorLoggingEvent.capture());
    final LoggingEvent loggingEvent = captorLoggingEvent.getValue();
    assertThat(loggingEvent.getLevel(), is(Level.ERROR));
    assertThat(loggingEvent.getMessage(), anyOf(is(
        "WPH1013E - com.ibm.whc.deid.utils.log.LogManager - logError - IPV-core error : \"This is test message\""),
        is("WPH1013E - com.ibm.whc.deid.utils.log.LogManagerTest - testLogError - IPV-core error : \"This is test message\"")));
  }

  @Test
  public void testLogDebug() {
    log.logDebug(LogCodes.WPH1015D, "This is test message");

    verify(mockAppender).doAppend(captorLoggingEvent.capture());
    final LoggingEvent loggingEvent = captorLoggingEvent.getValue();
    assertThat(loggingEvent.getLevel(), is(Level.DEBUG));
    assertThat(loggingEvent.getMessage(), anyOf(is(
        "WPH1015D - com.ibm.whc.deid.utils.log.LogManager - logError - IPV-core error : \"This is test message\""),
        is("WPH1015D - com.ibm.whc.deid.utils.log.LogManagerTest - testLogDebug - The parameter \"This is test message\" processed by class \"{1}\" is not found or invalid")));
  }
}
