/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.utils.log;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.slf4j.LoggerFactory;
import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;

public class LogManagerTest {

  private static final LogManager logManager = LogManager.getInstance();

  /*
   * Use slf4j to get a logger instance. Use the same class as was used by LogManager. The same
   * concrete object should be returned. The application uses slf4j to get access to logging, either
   * directly or through the LogManager class. The actual logger object returned will be based on
   * logback, per the application's logging configuration. Cast it appropriately so logging events
   * can be intercepted for testing.
   */
  private static final Logger appLogger = (Logger) LoggerFactory.getLogger(LogManager.class);


  @Test
  public void testLogInfo() {
    ListAppender<ILoggingEvent> appender = new ListAppender<>();
    appender.start();
    appLogger.addAppender(appender);

    try {
      logManager.logInfo(LogCodes.WPH1000I, "This is test message");

      assertEquals(1, appender.list.size());
      ILoggingEvent event = appender.list.get(0);
      // note - current implementation gets source class and method for the message from the stack
      String msg = event.getFormattedMessage();
      assertEquals(
          "WPH1000I - com.ibm.whc.deid.utils.log.LogManagerTest - testLogInfo - IPV-core information : \"This is test message\"",
          msg);
      assertEquals(Level.INFO, event.getLevel());
      System.out.println(msg);
    } finally {
      appLogger.detachAppender(appender);
      appender.stop();
    }
  }

  @Test
  public void testLogWarn() {
    ListAppender<ILoggingEvent> appender = new ListAppender<>();
    appender.start();
    appLogger.addAppender(appender);

    try {
      logManager.logWarn(LogCodes.WPH1012W, "This is test message");

      assertEquals(1, appender.list.size());
      ILoggingEvent event = appender.list.get(0);
      // note - current implementation gets source class and method for the message from the stack
      String msg = event.getFormattedMessage();
      assertEquals(
          "WPH1012W - com.ibm.whc.deid.utils.log.LogManagerTest - testLogWarn - IPV-core warning : \"This is test message\"",
          msg);
      assertEquals(Level.WARN, event.getLevel());
      System.out.println(msg);
    } finally {
      appLogger.detachAppender(appender);
      appender.stop();
    }
  }

  @Test
  public void testLogError() {
    ListAppender<ILoggingEvent> appender = new ListAppender<>();
    appender.start();
    appLogger.addAppender(appender);

    try {
      logManager.logError(LogCodes.WPH1013E, "This is test message");

      assertEquals(1, appender.list.size());
      ILoggingEvent event = appender.list.get(0);
      // note - current implementation gets source class and method for the message from the stack
      String msg = event.getFormattedMessage();
      assertEquals(
          "WPH1013E - com.ibm.whc.deid.utils.log.LogManagerTest - testLogError - IPV-core error : \"This is test message\"",
          msg);
      assertEquals(Level.ERROR, event.getLevel());
      System.out.println(msg);
    } finally {
      appLogger.detachAppender(appender);
      appender.stop();
    }
  }

  @Test
  public void testLogDebug() {
    ListAppender<ILoggingEvent> appender = new ListAppender<>();
    appender.start();
    appLogger.addAppender(appender);

    try {
      logManager.logDebug(LogCodes.WPH1015D, "This is test message");

      assertEquals(1, appender.list.size());
      ILoggingEvent event = appender.list.get(0);
      // note - current implementation gets source class and method for the message from the stack
      String msg = event.getFormattedMessage();
      assertEquals(
          "WPH1015D - com.ibm.whc.deid.utils.log.LogManagerTest - testLogDebug - The parameter \"This is test message\" processed by class \"{1}\" is not found or invalid",
          msg);
      assertEquals(Level.DEBUG, event.getLevel());
      System.out.println(msg);
    } finally {
      appLogger.detachAppender(appender);
      appender.stop();
    }
  }
}
