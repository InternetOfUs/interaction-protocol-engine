/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine;

import eu.internetofus.common.vertx.AbstractMainVerticle;
import eu.internetofus.wenet_interaction_protocol_engine.api.APIVerticle;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.PersistenceVerticle;
import eu.internetofus.wenet_interaction_protocol_engine.services.ServicesVerticle;
import io.vertx.core.AbstractVerticle;

/**
 * The Main verticle that deploy the necessary verticles for the WeNet
 * interactiomn protocol engine.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class MainVerticle extends AbstractMainVerticle {

  /**
   * {@inheritDoc}
   */
  @SuppressWarnings("unchecked")
  @Override
  protected Class<? extends AbstractVerticle>[] getVerticleClassesToDeploy() {

    return new Class[] { ServicesVerticle.class, PersistenceVerticle.class, APIVerticle.class, EngineWorker.class };

  }

}
