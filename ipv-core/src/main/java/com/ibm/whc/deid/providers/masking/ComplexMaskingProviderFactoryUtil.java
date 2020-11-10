/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.DeidConfig;
import com.ibm.whc.deid.shared.util.ConfigUtil;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class ComplexMaskingProviderFactoryUtil {

  private static DeidConfig deidConfig = null;

  private static ComplexMaskingProviderFactory complexMaskingProviderFactory = null;

  private static LogManager log = LogManager.getInstance();

  // get the static config object for the environment
  private static DeidConfig getDeidConfig() throws DeidException {
    if (deidConfig == null) {
      deidConfig = ConfigUtil.getDeidConfig();
    }
    return deidConfig;
  }

  public static ComplexMaskingProviderFactory getComplexMaskingProviderFactory() {

    if (complexMaskingProviderFactory == null) {
      try {
        DeidConfig deidConfig = getDeidConfig();

        @SuppressWarnings("unchecked")
        Constructor<? extends ComplexMaskingProviderFactory> constructor =
            (Constructor<? extends ComplexMaskingProviderFactory>) Class
                .forName(deidConfig.getComplexMaskingProviderFactoryClass()).getConstructor();
        complexMaskingProviderFactory = constructor.newInstance();
      } catch (NoSuchMethodException | ClassNotFoundException | IllegalAccessException
          | InstantiationException | InvocationTargetException | DeidException e) {
        log.logError(LogCodes.WPH1013E, e);
        throw new RuntimeException(e.getMessage());
      }
    }
    return complexMaskingProviderFactory;
  }

}
