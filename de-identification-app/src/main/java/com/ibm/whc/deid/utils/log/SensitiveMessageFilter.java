/*
 * (C) Copyright IBM Corp. 2021
 * 
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.utils.log;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.ibm.whc.deid.ObjectMapperFactory;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.spi.FilterReply;

//@formatter:off
/**
 * Logback filter that prevents a configurable list of message from being logged, based on the
 * logger name or message content. Configuration is a list of MessageDefinition items, each
 * containing the logger name and a message content pattern (as per Pattern class) to block. Both
 * must match an incoming message for that message to be blocked. In a MessageDefintiion, a logger
 * name or message content pattern that is <i>null</i> matches all incoming messages. Therefore,
 * messages with certain content can be blocked regardless of the logger that issued them and the
 * messages from a given logger can be blocked regardless of their content.
 * 
 * <p>
 * The filter and its configured are specified in the logback.xml file. Here is an example: 
 * <xmp>
 *  <appender name="stdout" class="ch.qos.logback.core.ConsoleAppender">
 *     <filter class="com.ibm.whc.deid.utils.log.SensitiveMessageFilter">
 *         <blockedMessages>{&quot;blockedMessages&quot;:[{&quot;loggerName&quot;:&quot;org.springframework.*HttpEntityMethodProcessor&quot;, &quot;message&quot;:&quot;Writing.*&quot;}]}</blockedMessages>
 *     </filter>   
 * </xmp>
 * 
 * @see java.util.regex.Pattern
 */
//@formatter:on
public class SensitiveMessageFilter extends Filter<ILoggingEvent> {
  
  public static class SensitiveMessageFilterConfig {
    
    private List<MessageDefinition> blockedMessages = null;

    public List<MessageDefinition> getBlockedMessages() {
      return blockedMessages;
    }

    public void setBlockedMessages(List<MessageDefinition> blockedMessages) {
      this.blockedMessages = blockedMessages;
    }    
  }

  public static class MessageDefinition {

    @JsonIgnore
    private Pattern loggerNamePattern = null;
    @JsonIgnore
    private Pattern messagePattern = null;
    
    public String getLoggerName() {
      return loggerNamePattern == null ? null : loggerNamePattern.pattern();
    }

    public void setLoggerName(String name) {
      loggerNamePattern = name == null ? null : Pattern.compile(name);
    }
    
    public String getMessage() {
      return messagePattern == null ? null : messagePattern.pattern();
    }
    
    public void setMessage(String message) {
      messagePattern = message == null ? null : Pattern.compile(message);
    }

    public boolean isMatch(ILoggingEvent event) {
      return (loggerNamePattern == null ||
          loggerNamePattern.matcher(event.getLoggerName()).matches()) &&
          (messagePattern == null || messagePattern.matcher(event.getMessage()).matches());
    }
  }

  private SensitiveMessageFilterConfig config = null;

  @Override
  public FilterReply decide(ILoggingEvent event) {
    FilterReply reply = FilterReply.NEUTRAL;
    if (config != null) {
      List<MessageDefinition> blockedMessages = config.getBlockedMessages();
      if (blockedMessages != null) {
        for (MessageDefinition blockedMessage : blockedMessages) {
          if (blockedMessage != null) {
            if (blockedMessage.isMatch(event)) {
              reply = FilterReply.DENY;
              break;
            }
          }
        }
      }
    }
    return reply;
  }
  
  public void setBlockedMessages(String json) {
    if (json == null) {
      config = null;
    } else {
      try {
        config = ObjectMapperFactory.getObjectMapper().readValue(json, SensitiveMessageFilterConfig.class);
      } catch (Exception e) {
        System.err
            .println(getClass().getName() + " configuration cannot be parsed - logging suppressed: "
                + e.getMessage() + ".  Full input string: " + json);
        // error reading config - block everything
        MessageDefinition bm = new MessageDefinition();
        bm.setLoggerName(null);
        bm.setMessage(null);
        List<MessageDefinition> blockedMessages = new ArrayList<>();
        blockedMessages.add(bm);
        config = new SensitiveMessageFilterConfig();
        config.setBlockedMessages(blockedMessages);
      }
    }
  }      
}
