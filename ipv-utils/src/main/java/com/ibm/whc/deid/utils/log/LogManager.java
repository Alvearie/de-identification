/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.utils.log;

import java.io.Serializable;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.UUID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

/*
 * LogManager uses to logging info, warning, errors logs to WPHDeid.log file It uses logback.xml to
 * configure the log utilities
 */
public class LogManager implements Serializable {

  /** */
  private static final long serialVersionUID = -1338205196777167297L;

  private static Logger log = LoggerFactory.getLogger(LogManager.class);

  private static LogManager instance = null;
  private HashMap<String, LogProperties> logMap = new HashMap<String, LogProperties>();

  private String localIpAddress = "";

  private static final String MDC_IPADDRESS = "ipaddress";

  static {
    try {
      LogManager.getInstance().initializedLogManager();
    } catch (Exception e) {
      log.warn("Fail to initialized logManager due to " + e.getMessage());
    }
  }

  protected LogManager() {}

  public static LogManager getInstance() {
    if (instance == null) {
      instance = new LogManager();
    }
    return instance;
  }

  /*
   * Retrieve the IP Address from machine that the job is running on
   */
  public String getLocalIpAddress() {
    try {
      InetAddress inetAddress = InetAddress.getLocalHost();
      localIpAddress = inetAddress.getHostAddress();
    } catch (UnknownHostException e) {
      log.warn("Error when getting host ip address. e: " + e.getMessage());
    }
    return localIpAddress;
  }

  /*
   * Adding log properties for a certain uuid TODO: currently we are not using tenant ID value in
   * MDC.
   */
  public void addNewLogProperties(String uuid) throws Exception {
    LogProperties logProperties = new LogProperties();
    logProperties.setLocalIpAddress(getLocalIpAddress());
    logMap.put(uuid, logProperties);
  }

  /*
   * Retrieve the log properties for a certain uuid No tenant ID value in MDC, move it to log
   * message
   */
  public void getLogPropertiesAndApply(String uuid) throws Exception {
    LogProperties logProperties = logMap.get(uuid);
    MDC.put(MDC_IPADDRESS, logProperties.getLocalIpAddress());
  }

  public void clearLogMDC() {
    MDC.clear();
  }

  /*
   * uuid for each log instance
   */
  public String getUuid() {
    return UUID.randomUUID().toString();
  }

  /*
   * Getting log properties for each run of application
   */
  public void initializedLogManager() throws Exception {
    String uuid = getUuid();
    LogManager.getInstance().addNewLogProperties(uuid);
    LogManager.getInstance().getLogPropertiesAndApply(uuid);
  }

  /*
   * Generate INFO log
   */
  public void logInfo(final String msgkey, Object... msgs) {
    if (log.isInfoEnabled()) {
      String messageBundle = getMessage(msgkey, msgs).toString();
      log.info(messageBundle);
    }
  }

  public void logInfo(final String msgkey, Exception e, Object... msgs) {
    if (log.isInfoEnabled()) {
      if (msgs == null || msgs.length == 0) {
        msgs = new Object[1];
        msgs[0] = e.getMessage();
      }
      String messageBundle = getMessage(msgkey, msgs).toString();
      log.info(messageBundle, e);
    }
  }

  public void logDebug(final String msgs, Object... objs) {
    if (log.isDebugEnabled()) {
      String messageBundle = getMessage(msgs, objs).toString();
      log.debug(messageBundle);
    }
  }

  public void logDebug(final String msgs, Exception e, Object... objs) {
    if (log.isDebugEnabled()) {
      if (objs == null || objs.length == 0) {
        objs = new Object[1];
        objs[0] = e.getMessage();
      }
      String messageBundle = getMessage(msgs, objs).toString();
      log.debug(messageBundle, e);
    }
  }

  /*
   * Generate WARN log
   */
  public void logWarn(final String msgkey, Object... msgs) {
    if (log.isWarnEnabled()) {
      String messageBundle = getMessage(msgkey, msgs).toString();
      log.warn(messageBundle);
    }
  }

  /*
   * Generate WARN log and log exception
   */
  public void logWarn(final String msgkey, Exception e, Object... msgs) {
    if (log.isWarnEnabled()) {
      if (msgs == null || msgs.length == 0) {
        msgs= new Object[1];
        msgs[0] = e.getMessage();
      }
      String messageBundle = getMessage(msgkey, msgs).toString();
      log.warn(messageBundle, e);
    }
  }

  /*
   * Generate ERROR log
   */
  public void logError(final String msgkey, Object... msgs) {
    if (log.isErrorEnabled()) {
      String messageBundle = getMessage(msgkey, msgs).toString();
      log.error(messageBundle);
    }
  }

  /*
   * Generate ERROR log and log exception
   */
  public void logError(final String msgkey, Exception e, Object... msgs) {
    if (log.isErrorEnabled()) {
      if (msgs == null || msgs.length == 0) {
        msgs = new Object[1];
        msgs[0] = e.getMessage();
      }
      String messageBundle = getMessage(msgkey, msgs).toString();
      log.error(messageBundle, e);
    }
  }

  /** Generates message for logger by concatenating on message key, stack trace, and  class name */
  protected StringBuilder getMessage(String msgkey, Object[] msgs) {
    StackTraceElement stackTraceElement = Thread.currentThread().getStackTrace()[3];
    StringBuilder messageBundle = new StringBuilder();
    messageBundle.append(msgkey).append(" - ").append(stackTraceElement.getClassName())
        .append(" - ").append(stackTraceElement.getMethodName()).append(" - ")
        .append(Messages.getMessage(msgkey, msgs));
    return messageBundle;
  }

  public boolean isDebugEnabled() {
    return log.isDebugEnabled();
  }

  public boolean isInfoEnabled() {
    return log.isInfoEnabled();
  }

  public boolean isWarnEnabled() {
    return log.isWarnEnabled();
  }

  public boolean isErrorEnabled() {
    return log.isErrorEnabled();
  }
}
