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

package eu.internetofus.wenet_interaction_protocol_engine.api.incentives;

import eu.internetofus.common.components.WeNetValidateContext;
import eu.internetofus.common.components.models.Incentive;
import eu.internetofus.common.model.Model;
import eu.internetofus.common.vertx.ServiceResponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.EngineWorker;
import eu.internetofus.wenet_interaction_protocol_engine.MessageForWorkerBuilder;
import eu.internetofus.wenet_interaction_protocol_engine.ProtocolData;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;
import javax.ws.rs.core.Response.Status;
import org.tinylog.Logger;

/**
 * Implementation of the {@link Incentives} services.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class IncentivesResource implements Incentives {

  /**
   * The event bus that is using.
   */
  protected Vertx vertx;

  /**
   * Create a new instance to provide the services of the {@link Incentives}.
   *
   * @param vertx with the event bus to use.
   */
  public IncentivesResource(final Vertx vertx) {

    this.vertx = vertx;

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void sendIncentive(final JsonObject body, final ServiceRequest context,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var incentive = Model.fromJsonObject(body, Incentive.class);
    if (incentive == null) {

      Logger.trace("Fail sendIncentive: {} is not a valid incentive to send", body);
      ServiceResponseHandlers.responseWithErrorMessage(resultHandler, Status.BAD_REQUEST, "bad_incentive",
          "Unexpected incentive model.");

    } else {

      incentive.validate(new WeNetValidateContext("bad_incentive", this.vertx)).onComplete(validate -> {

        if (validate.failed()) {

          final var cause = validate.cause();
          Logger.trace(cause, "Fail sendIncentive: {} does not contains a valid incentive", body);
          ServiceResponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

        } else {

          ProtocolData.createWith(incentive, this.vertx).onSuccess(protocol -> {

            final var message = MessageForWorkerBuilder.buildProtocolMessageForSendIncentive(incentive, protocol);
            this.vertx.eventBus().publish(EngineWorker.ADDRESSS, message);

          });

          Logger.trace("Accepted sendIncentive {} ", body);
          ServiceResponseHandlers.responseWith(resultHandler, Status.ACCEPTED, incentive);
        }

      });
    }
  }

}
