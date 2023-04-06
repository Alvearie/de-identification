/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.io.ByteArrayOutputStream;
import org.junit.After;
import org.junit.Before;
import org.slf4j.LoggerFactory;
import com.ibm.whc.deid.utils.log.LogManager;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.CoreConstants;
import ch.qos.logback.core.OutputStreamAppender;
import ch.qos.logback.core.encoder.EchoEncoder;


public class TestLogSetUp {

  /*
   * Use slf4j to get a logger instance. Use the same class as was used by LogManager. The same
   * concrete object should be returned. The application uses slf4j to get access to logging, either
   * directly or through the LogManager class. The actual logger object returned will be based on
   * logback, per the application's logging configuration. Cast it appropriately so logging events
   * can be intercepted for testing.
   */
  private static final Logger appLogger = (Logger) LoggerFactory.getLogger(LogManager.class);

  // accessed by subclasses to check log records
  public final ByteArrayOutputStream outContent = new ByteArrayOutputStream();

  private final OutputStreamAppender<ILoggingEvent> appender = new OutputStreamAppender<>();
  private final EchoEncoder<ILoggingEvent> encoder = new EchoEncoder<ILoggingEvent>() {
    @Override
    public byte[] encode(ILoggingEvent event) {
      // match the output to the layout returned by the previous
      // version of test utility that used log4j 1.2 returned
      StringBuilder sb = new StringBuilder();
      sb.append(event.getLevel()).append(" - ");
      sb.append(event.getFormattedMessage());
      sb.append(CoreConstants.LINE_SEPARATOR);
      return sb.toString().getBytes();
    }
  };

  // protected ObjectMapper objectMapper = new ObjectMapper();


  @Before
  public void setupAppender() {
    appender.setEncoder(encoder);
    appender.setOutputStream(outContent);
    appender.start();
    appLogger.addAppender(appender);
  }

  @After
  public void tearAppender() {
    appLogger.detachAppender(appender);
    appender.stop();
  }
}
