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

import javax.ws.rs.core.Response.Status;

import org.tinylog.Logger;

import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.incentive_server.Incentive;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolAddress;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolAddress.Component;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolMessage;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.vertx.OperationReponseHandlers;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.OperationRequest;
import io.vertx.ext.web.api.OperationResponse;

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
  public void sendIncentive(final JsonObject body, final OperationRequest context, final Handler<AsyncResult<OperationResponse>> resultHandler) {

    final var incentive = Model.fromJsonObject(body, Incentive.class);
    if (incentive == null) {

      Logger.trace("Fail sendIncentive: {} is not a valid incentive to send", body);
      OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.BAD_REQUEST, "bad_incentive", "Unexpected incentive model.");

    } else {

      incentive.validate("bad_incentive", this.vertx).onComplete(validate -> {

        if (validate.failed()) {

          final var cause = validate.cause();
          Logger.trace(cause, "Fail sendIncentive: {} does not contains a valid incentive", body);
          OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

        } else {

          final var message = new ProtocolMessage();
          message.appId = incentive.AppID;
          message.sender = new ProtocolAddress();
          message.sender.component = Component.INCENTIVE_SERVER;
          message.receiver = new ProtocolAddress();
          message.receiver.component = Component.INTERACTION_PROTOCOL_ENGINE;
          message.receiver.userId = incentive.UserId;
          message.particle = "INCENTIVE";
          message.content = incentive.toJsonObject();
          WeNetInteractionProtocolEngine.createProxy(this.vertx).sendMessage(message, send -> {

            if (send.failed()) {

              final var cause = send.cause();
              Logger.trace(cause, "Fail sendIncentive: {} of {} is not accepted", message, body);
              OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

            } else {

              Logger.trace("Accepted sendIncentive {} ", body);
              OperationReponseHandlers.responseWith(resultHandler, Status.ACCEPTED, incentive);
            }

          });
        }

      });
    }
  }

}
