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

package eu.internetofus.wenet_interaction_protocol_engine.api.profiles;

import eu.internetofus.common.vertx.ServiceResponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.InteractionsRepository;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.StatesRepository;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;
import org.tinylog.Logger;

/**
 * Resource that implements the web services defined at {@link Profiles}.
 *
 * @see Profiles
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class ProfilesResource implements Profiles {

  /**
   * The event bus that is using.
   */
  protected Vertx vertx;

  /**
   * Create a new instance to provide the services of the {@link Profiles}.
   *
   * @param vertx with the event bus to use.
   */
  public ProfilesResource(final Vertx vertx) {

    this.vertx = vertx;

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void profileDeleted(final String profileId, final ServiceRequest request,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    ServiceResponseHandlers.responseOk(resultHandler);

    InteractionsRepository.createProxy(this.vertx).deleteAllInteractionByUser(profileId).onComplete((deleted) -> {

      if (deleted.failed()) {

        Logger.trace(deleted.cause(), "Cannot delete the interactions involving the user {}.", profileId);

      }

    });

    StatesRepository.createProxy(this.vertx).deleteAllStateByUser(profileId).onComplete((deleted) -> {

      if (deleted.failed()) {

        Logger.trace(deleted.cause(), "Cannot delete the states associated to the profile {}.", profileId);

      }

    });

  }

}
