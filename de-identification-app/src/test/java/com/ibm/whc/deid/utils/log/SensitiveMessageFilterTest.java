/*
 * © Merative US L.P. 2021
 * 
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.utils.log;

import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import java.util.Map;
import org.junit.Test;
import org.slf4j.Marker;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.utils.log.SensitiveMessageFilter.MessageDefinition;
import com.ibm.whc.deid.utils.log.SensitiveMessageFilter.SensitiveMessageFilterConfig;
import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.classic.spi.IThrowableProxy;
import ch.qos.logback.classic.spi.LoggerContextVO;
import ch.qos.logback.core.spi.FilterReply;

public class SensitiveMessageFilterTest {

  private class TestLoggingEvent implements ILoggingEvent {

    private final String loggerName;
    private final String message;

    public TestLoggingEvent(String name, String msg) {
      loggerName = name;
      message = msg;
    }

    @Override
    public String getThreadName() {
      return "t1";
    }

    @Override
    public Level getLevel() {
      return Level.INFO;
    }

    @Override
    public String getMessage() {
      return message;
    }

    @Override
    public Object[] getArgumentArray() {
      return null;
    }

    @Override
    public String getFormattedMessage() {
      return "msg1";
    }

    @Override
    public String getLoggerName() {
      return loggerName;
    }

    @Override
    public LoggerContextVO getLoggerContextVO() {
      return null;
    }

    @Override
    public IThrowableProxy getThrowableProxy() {
      return null;
    }

    @Override
    public StackTraceElement[] getCallerData() {
      return null;
    }

    @Override
    public boolean hasCallerData() {
      return false;
    }

    @Override
    public Marker getMarker() {
      return null;
    }

    @Override
    public Map<String, String> getMDCPropertyMap() {
      return null;
    }

    @Override
    @SuppressWarnings("deprecation")
    public Map<String, String> getMdc() {
      return null;
    }

    @Override
    public long getTimeStamp() {
      return 0;
    }

    @Override
    public void prepareForDeferredProcessing() {
      // nothing required
    }
  }

  @Test
  public void testMain() throws JsonProcessingException {
    SensitiveMessageFilter filter = new SensitiveMessageFilter();

    // no messages configured - any message allowed
    assertEquals(FilterReply.NEUTRAL, filter.decide(new TestLoggingEvent("name", "msg")));
    assertEquals(FilterReply.NEUTRAL, filter.decide(new TestLoggingEvent(null, null))); // invalid

    // error in config - no message allowed
    filter.setBlockedMessages("{");
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("name", "msg")));

    // empty config - messages allowed
    SensitiveMessageFilterConfig config = new SensitiveMessageFilterConfig();
    filter.setBlockedMessages(ObjectMapperFactory.getObjectMapper().writeValueAsString(config));
    assertEquals(FilterReply.NEUTRAL, filter.decide(new TestLoggingEvent("name", "msg")));

    // empty list - messages allowed
    ArrayList<MessageDefinition> defs = new ArrayList<>();
    config.setBlockedMessages(defs);
    filter.setBlockedMessages(ObjectMapperFactory.getObjectMapper().writeValueAsString(config));
    assertEquals(FilterReply.NEUTRAL, filter.decide(new TestLoggingEvent("name", "msg")));

    // null message added - messages allowed
    defs.add(null);
    config = new SensitiveMessageFilterConfig();
    config.setBlockedMessages(defs);
    filter.setBlockedMessages(ObjectMapperFactory.getObjectMapper().writeValueAsString(config));
    assertEquals(FilterReply.NEUTRAL, filter.decide(new TestLoggingEvent("name", "msg")));

    // configured message with just logger name specified
    MessageDefinition def = new MessageDefinition();
    def.setLoggerName("logger1");
    defs = new ArrayList<>();
    defs.add(def);
    config = new SensitiveMessageFilterConfig();
    config.setBlockedMessages(defs);
    filter.setBlockedMessages(ObjectMapperFactory.getObjectMapper().writeValueAsString(config));
    assertEquals(FilterReply.NEUTRAL, filter.decide(new TestLoggingEvent("name", "msg")));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("logger1", "msg")));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("logger1", "msg2")));
    assertEquals(FilterReply.NEUTRAL, filter.decide(new TestLoggingEvent("logger2", "msg")));

    // configured message with just message specified
    def = new MessageDefinition();
    def.setMessage("avoid this message");
    defs.add(def);
    config.setBlockedMessages(defs);
    filter.setBlockedMessages(ObjectMapperFactory.getObjectMapper().writeValueAsString(config));
    assertEquals(FilterReply.NEUTRAL, filter.decide(new TestLoggingEvent("name", "msg")));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("logger1", "msg")));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("logger1", "msg2")));
    assertEquals(FilterReply.NEUTRAL, filter.decide(new TestLoggingEvent("logger2", "msg")));
    assertEquals(FilterReply.DENY,
        filter.decide(new TestLoggingEvent("Logger2", "avoid this message")));
    assertEquals(FilterReply.DENY,
        filter.decide(new TestLoggingEvent("Logger1", "avoid this message")));
    assertEquals(FilterReply.DENY,
        filter.decide(new TestLoggingEvent("name", "avoid this message")));

    // configured message with name and message specified
    def = new MessageDefinition();
    def.setLoggerName("logger3");
    def.setMessage("bad message.*");
    defs.add(def);
    config.setBlockedMessages(defs);
    filter.setBlockedMessages(ObjectMapperFactory.getObjectMapper().writeValueAsString(config));
    assertEquals(FilterReply.NEUTRAL, filter.decide(new TestLoggingEvent("name", "msg")));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("logger1", "msg")));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("logger1", "msg2")));
    assertEquals(FilterReply.NEUTRAL, filter.decide(new TestLoggingEvent("logger2", "msg")));
    assertEquals(FilterReply.DENY,
        filter.decide(new TestLoggingEvent("Logger2", "avoid this message")));
    assertEquals(FilterReply.DENY,
        filter.decide(new TestLoggingEvent("Logger1", "avoid this message")));
    assertEquals(FilterReply.DENY,
        filter.decide(new TestLoggingEvent("name", "avoid this message")));
    assertEquals(FilterReply.NEUTRAL,
        filter.decide(new TestLoggingEvent("logger3", "Bad Message")));
    assertEquals(FilterReply.NEUTRAL,
        filter.decide(new TestLoggingEvent("logger2", "bad message")));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("logger3", "bad message")));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("logger3", "bad message x")));

    // configured message with both null - nothing allowed
    def = new MessageDefinition();
    defs.add(def);
    config.setBlockedMessages(defs);
    filter.setBlockedMessages(ObjectMapperFactory.getObjectMapper().writeValueAsString(config));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("name", "msg")));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("logger1", "msg")));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("logger1", "msg2")));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("logger2", "msg")));
    assertEquals(FilterReply.DENY,
        filter.decide(new TestLoggingEvent("Logger2", "avoid this message")));
    assertEquals(FilterReply.DENY,
        filter.decide(new TestLoggingEvent("Logger1", "avoid this message")));
    assertEquals(FilterReply.DENY,
        filter.decide(new TestLoggingEvent("name", "avoid this message")));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("logger3", "Bad Message")));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("logger2", "bad message")));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("logger3", "bad message")));
    assertEquals(FilterReply.DENY, filter.decide(new TestLoggingEvent("logger3", "bad message x")));

    // config reset - everything allowed
    filter.setBlockedMessages(null);
    assertEquals(FilterReply.NEUTRAL, filter.decide(new TestLoggingEvent("name", "msg")));
    assertEquals(FilterReply.NEUTRAL, filter.decide(new TestLoggingEvent("logger1", "msg")));
    assertEquals(FilterReply.NEUTRAL, filter.decide(new TestLoggingEvent("logger1", "msg2")));
    assertEquals(FilterReply.NEUTRAL, filter.decide(new TestLoggingEvent("logger2", "msg")));
    assertEquals(FilterReply.NEUTRAL,
        filter.decide(new TestLoggingEvent("Logger2", "avoid this message")));
    assertEquals(FilterReply.NEUTRAL,
        filter.decide(new TestLoggingEvent("Logger1", "avoid this message")));
    assertEquals(FilterReply.NEUTRAL,
        filter.decide(new TestLoggingEvent("name", "avoid this message")));
    assertEquals(FilterReply.NEUTRAL,
        filter.decide(new TestLoggingEvent("logger3", "Bad Message")));
    assertEquals(FilterReply.NEUTRAL,
        filter.decide(new TestLoggingEvent("logger2", "bad message")));
    assertEquals(FilterReply.NEUTRAL,
        filter.decide(new TestLoggingEvent("logger3", "bad message")));
    assertEquals(FilterReply.NEUTRAL,
        filter.decide(new TestLoggingEvent("logger3", "bad message x")));
  }
}
