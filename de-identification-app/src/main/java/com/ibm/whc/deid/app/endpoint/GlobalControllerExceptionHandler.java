/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint;

import javax.servlet.http.HttpServletRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import com.ibm.whc.deid.endpoint.exception.BadRequestException;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.exception.InvalidInputException;
import com.ibm.whc.deid.utils.log.Messages;

/*
 * This class handles the rest response statuses and content for certain exception types
 */
@ControllerAdvice
public class GlobalControllerExceptionHandler {

  @ResponseBody
  @ExceptionHandler(DeidException.class)
  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  String handleServerError(DeidException ex) {
    return ex.getMessage();
  }

  @ResponseBody
  @ExceptionHandler(RuntimeException.class)
  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  String handleServerError(RuntimeException ex) {
    return Messages.getMessage("WPH6001E");
  }

  @ResponseBody
  @ExceptionHandler(InvalidInputException.class)
  @ResponseStatus(HttpStatus.BAD_REQUEST)
  String handleServerError(InvalidInputException ex) {
    return ex.getMessage();
  }

  @ResponseBody
  @ExceptionHandler(HttpMessageNotReadableException.class)
  @ResponseStatus(HttpStatus.BAD_REQUEST)
  String handleServerError(HttpMessageNotReadableException ex) {
    return ex.getMessage();
  }

  @ResponseBody
  @ExceptionHandler(IllegalArgumentException.class)
  @ResponseStatus(HttpStatus.BAD_REQUEST)
  String handleClientError(IllegalArgumentException ex) {
    return ex.getMessage();
  }

  @ResponseBody
  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler(BadRequestException.class)
  String handleBadRequest(HttpServletRequest req, Exception ex) {
    return ex.getMessage();
  }
}
