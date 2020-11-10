/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.utils.log.LogManager;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import org.apache.log4j.Appender;
import org.apache.log4j.Layout;
import org.apache.log4j.Logger;
import org.apache.log4j.SimpleLayout;
import org.apache.log4j.WriterAppender;
import org.junit.After;
import org.junit.Before;

public class TestLogSetUp {

  private static final String APPENDER_NAME = "log4jRuleAppender";
  private static final Layout LAYOUT = new SimpleLayout();

  public Logger logger;
  public ByteArrayOutputStream outContent = new ByteArrayOutputStream();

  protected ObjectMapper objectMapper = new ObjectMapper();

  @Before
  public void setupAppender() {
    logger = Logger.getLogger(LogManager.class);
    Appender appender = new WriterAppender(LAYOUT, outContent);
    appender.setName(APPENDER_NAME);
    logger.addAppender(appender);
  }

  @After
  public void tearAppender() throws IOException {
    logger.removeAllAppenders();
    outContent.flush();
    outContent.close();
  }

  protected void printJsonObject(Object json) {
    try {
      System.out.println(objectMapper.writeValueAsString(json));
    } catch (JsonProcessingException e) {
      // ignore exception
    }
  }
}
