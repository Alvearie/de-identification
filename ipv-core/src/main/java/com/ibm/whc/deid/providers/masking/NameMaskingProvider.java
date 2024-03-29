/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.security.SecureRandom;
import com.ibm.whc.deid.models.FirstName;
import com.ibm.whc.deid.models.LastName;
import com.ibm.whc.deid.shared.pojo.config.masking.NameMaskingProviderConfig;
import com.ibm.whc.deid.util.NamesManager;

/**
 * The type Name masking provider.
 *
 */
public class NameMaskingProvider extends AbstractMaskingProvider {

  private static final long serialVersionUID = 3798105506081000286L;

  private static final String[] initials = {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
      "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"};
  
  private final boolean allowUnisex;
  private final boolean genderPreserve;
  private final boolean getPseudorandom;

  protected transient volatile NamesManager namesResourceManager = null;

  public NameMaskingProvider(NameMaskingProviderConfig configuration, String tenantId,
      String localizationProperty) {
    super(tenantId, localizationProperty, configuration);
    this.random = new SecureRandom();
    this.allowUnisex = configuration.isMaskingAllowUnisex();
    this.genderPreserve = configuration.isMaskGenderPreserve();
    this.getPseudorandom = configuration.isMaskPseudorandom();
  }

  @Override
  public String mask(String identifier) {
    if (identifier == null) {
      debugFaultyInput("identifier");
      return null;
    }

    NamesManager names = getNamesManager();
    
    StringBuilder builder = new StringBuilder();

    for (String token : identifier.split("\\b")) {
      token = token.trim();

      if (token.isEmpty())
        continue;

      final String maskedToken;

      if (1 == token.length()) {
        if (Character.isAlphabetic(token.charAt(0))) {
          // initial: randomize it
          maskedToken = initials[random.nextInt(initials.length)];
        } else {
          // preserve it
          maskedToken = token;
        }
      } else {
        FirstName lookup = names.getFirstName(token);
        if (lookup != null) {
          if (getPseudorandom) {
            maskedToken = names.getPseudoRandomFirstName(lookup.getGender(), allowUnisex, token);
          } else if (!genderPreserve) {
            if (allowUnisex)
              maskedToken = names.getRandomFirstNameWithoutPreservingGender(true,
                  lookup.getNameCountryCode());
            else
              maskedToken = names.getRandomFirstNameWithoutPreservingGender(false,
                  lookup.getNameCountryCode());
          } else {
            maskedToken = names.getRandomFirstName(lookup.getGender(), allowUnisex,
                lookup.getNameCountryCode());
          }
        } else {
          LastName lookupLastName = names.getLastName(token);
          if (lookupLastName != null) {
            if (getPseudorandom) {
              maskedToken = names.getPseudoRandomLastName(token);
            } else {
              maskedToken = names.getRandomLastName(lookupLastName.getNameCountryCode());
            }
          } else {
            return applyUnexpectedValueHandling(identifier, () -> {
              String randomName;
              if (getPseudorandom) {
                // For other privacy providers, requesting pseudorandom masking means
                // unexpected value handling does not apply, since the input does not
                // need to be recognized to apply pseudorandom masking. Therefore,
                // those providers do not look at any pseudorandom configuration option
                // when generating random values during unexpected value handling. It
                // is consistent to look at the pseudorandom setting here, though, since
                // this is generating a complete replacement for the name and had it been
                // known that complete replacement of the name was required, pseudorandom
                // would have been used to do that if it had been configured.
                randomName = names.getPseudoRandomFirstName(identifier) + " "
                    + names.getPseudoRandomLastName(identifier);
              } else {
                randomName = names.getRandomFirstName() + " " + names.getRandomLastName();
              }
              return randomName;
            });
          }
        }
      }

      builder.append(maskedToken);
      builder.append(' ');
    }

    return builder.toString().trim();
  }

  protected NamesManager getNamesManager() {
    if (namesResourceManager == null) {
      namesResourceManager = NamesManager.buildNamesManager(tenantId,  localizationProperty);
    }
    return namesResourceManager;
  }
}
