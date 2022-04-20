/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.identifiers;

import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.LogManager;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Objects;
import java.util.Properties;
import java.util.Set;

public class IdentifierFactoryHelper implements Serializable {
  private static final long serialVersionUID = 9183243223063761662L;

  private static LogManager log = LogManager.getInstance();
  private Set<Identifier> identifiers = new HashSet<Identifier>();

  @SuppressWarnings("unchecked")
  public IdentifierFactoryHelper(final InputStream is) {
    if (Objects.nonNull(is)) {
      try {
        Properties properties = new Properties();
        properties.load(is);
        for (Enumeration<?> names = properties.propertyNames(); names.hasMoreElements();) {
          String name = (String) names.nextElement();

          registerIdentifier((Class<? extends Identifier>) Class.forName(name));
        }
      } catch (IOException | ClassNotFoundException | InstantiationException
          | IllegalAccessException | IllegalArgumentException | InvocationTargetException
          | NoSuchMethodException | SecurityException e) {
        log.logError(LogCodes.WPH1013E, e);
      }
    }
  }

  @SuppressWarnings("unchecked")
  public IdentifierFactoryHelper(Set<String> identifierClassNames) {
    identifierClassNames.forEach(name -> {
      try {
        registerIdentifier((Class<? extends Identifier>) Class.forName(name));
      } catch (IllegalAccessException | InstantiationException | ClassNotFoundException
          | IllegalArgumentException | InvocationTargetException | NoSuchMethodException
          | SecurityException e) {
        log.logError(LogCodes.WPH1013E, e);
      }
    });
  }

  /**
   * Available identifiers collection.
   *
   * @return the collection
   */
  public Collection<Identifier> availableIdentifiers() {
    return identifiers;
  }

  /**
   * Register identifier.
   *
   * @param identifierType the identifier type
   * @throws IllegalAccessException the illegal access exception
   * @throws InstantiationException the instantiation exception
   * @throws SecurityException
   * @throws NoSuchMethodException
   * @throws InvocationTargetException
   * @throws IllegalArgumentException
   */
  public void registerIdentifier(final Class<? extends Identifier> identifierType)
      throws IllegalAccessException, InstantiationException, IllegalArgumentException,
      InvocationTargetException, NoSuchMethodException, SecurityException {
    registerIdentifier(identifierType.getDeclaredConstructor().newInstance());
  }

  /**
   * Register identifier.
   *
   * @param identifier the identifier
   */
  public void registerIdentifier(final Identifier identifier) {
    identifiers.add(identifier);
  }
}
