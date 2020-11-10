/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Map;

import com.ibm.whc.deid.providers.ProviderType;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.DeidConfig;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.util.ConfigUtil;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;

public class MaskingProviderFactoryUtil {

  private static DeidConfig deidConfig = null;

  private static MaskingProviderFactory maskingProviderFactory = null;

  private static LogManager log = LogManager.getInstance();

  // get the static config object for the environment
  private static DeidConfig getDeidConfig() throws DeidException {
    if (deidConfig == null) {
      deidConfig = ConfigUtil.getDeidConfig();
    }
    return deidConfig;
  }

  public static MaskingProviderFactory getMaskingProviderFactory() {

    if (maskingProviderFactory == null) {
      try {
        DeidConfig deidConfig = getDeidConfig();

        @SuppressWarnings("unchecked")
        Constructor<? extends MaskingProviderFactory> constructor =
            (Constructor<? extends MaskingProviderFactory>) Class
                .forName(deidConfig.getMaskingProviderFactoryClass()).getConstructor();
        maskingProviderFactory = constructor.newInstance();
      } catch (NoSuchMethodException | ClassNotFoundException | IllegalAccessException
          | InstantiationException | InvocationTargetException | DeidException e) {
        log.logError(LogCodes.WPH1013E, e);
        throw new RuntimeException(e.getMessage());
      }
    }
    return maskingProviderFactory;
  }

  public static MaskingProviderFactory getNewMaskingProviderFactory(
      DeidMaskingConfig deidMaskingConfig, Map<String, ProviderType> identifiedTypes) {
    MaskingProviderFactory customMaskingProviderFactory;
    try {
      DeidConfig deidConfig = getDeidConfig();

      @SuppressWarnings("unchecked")
      Constructor<? extends MaskingProviderFactory> constructor =
          (Constructor<? extends MaskingProviderFactory>) Class
              .forName(deidConfig.getMaskingProviderFactoryClass())
							.getConstructor(DeidMaskingConfig.class, Map.class);
      customMaskingProviderFactory =
					constructor.newInstance(deidMaskingConfig, identifiedTypes);
    } catch (NoSuchMethodException | ClassNotFoundException | IllegalAccessException
        | InstantiationException | InvocationTargetException | DeidException e) {
      log.logError(LogCodes.WPH1013E, e);
      throw new RuntimeException(e.getMessage());
    }
    return customMaskingProviderFactory;
  }

}
