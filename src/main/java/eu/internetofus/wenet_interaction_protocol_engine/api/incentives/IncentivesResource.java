/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api.incentives;

import eu.internetofus.common.model.Model;
import eu.internetofus.common.components.models.Incentive;
import eu.internetofus.common.vertx.ServiceResponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.EngineWorker;
import eu.internetofus.wenet_interaction_protocol_engine.HardCodedProtocolWorker;
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

      incentive.validate("bad_incentive", this.vertx).onComplete(validate -> {

        if (validate.failed()) {

          final var cause = validate.cause();
          Logger.trace(cause, "Fail sendIncentive: {} does not contains a valid incentive", body);
          ServiceResponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

        } else {

          ProtocolData.createWith(incentive, this.vertx).onSuccess(protocol -> {

            if (!protocol.hasProtocolNorms()) {

              final var message = MessageForWorkerBuilder.buildSendIncentiveMessage(incentive);
              this.vertx.eventBus().publish(HardCodedProtocolWorker.ADDRESSS, message);

            } else {

              final var message = MessageForWorkerBuilder.buildProtocolMessageForSendIncentive(incentive, protocol);
              this.vertx.eventBus().publish(EngineWorker.ADDRESSS, message);
            }

          });

          Logger.trace("Accepted sendIncentive {} ", body);
          ServiceResponseHandlers.responseWith(resultHandler, Status.ACCEPTED, incentive);
        }

      });
    }
  }

}
