/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import java.util.List;

import com.ibm.whc.deid.models.Occupation;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.pojo.config.masking.OccupationMaskingProviderConfig;
import com.ibm.whc.deid.util.ManagerFactory;
import com.ibm.whc.deid.util.OccupationManager;

/**
 * Privacy provider to mask the names of occupations.
 */
public class OccupationMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 118800423304584769L;

  protected final boolean generalizeToCategory;

  protected transient volatile OccupationManager occupationResourceManager = null;

  public OccupationMaskingProvider(OccupationMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty, configuration);
    this.generalizeToCategory = configuration.isMaskGeneralize();
    this.random = new SecureRandom();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    OccupationManager occupationManager = getOccupationManager();

    if (this.generalizeToCategory) {
      Occupation occupation = occupationManager.getValue(identifier);

      if (occupation == null) {
        return applyUnexpectedValueHandling(identifier,
            () -> getRandomOccupationName(occupationManager));
      }

      List<String> categories = occupation.getCategories();
      int count = categories.size();
      if (count == 0) {
        return null;
      }
      if (count == 1) {
        return categories.get(0);
      }
      int randomIndex = random.nextInt(count);
      return categories.get(randomIndex);
    }

    return getRandomOccupationName(occupationManager);
  }

  protected String getRandomOccupationName(OccupationManager occupationManager) {
    Occupation occ = occupationManager.getRandomValue();
    return occ == null ? null : occ.getName();
  }

  protected OccupationManager getOccupationManager() {
    if (occupationResourceManager == null) {
      occupationResourceManager = (OccupationManager) ManagerFactory.getInstance()
          .getManager(tenantId, Resource.OCCUPATION, null, localizationProperty);
    }
    return occupationResourceManager;
  }
}
